module Loop = struct
  open Unix

  module ProtocolWorkers = struct
    let reply (server_descriptor, socket_fd, _, message) =
      let open Lib.Message in
      let open Lib.Server in
      if message.ciaddr <> inet_addr_any
      then (
        let bytes = bytes_of_message message in
        let recipient_addr = ADDR_INET (message.ciaddr, server_descriptor.port) in
        let len_sent = sendto socket_fd bytes 0 (Bytes.length bytes) [] recipient_addr in
        Printf.eprintf "[reply] sent back %d bytes\n" len_sent)
      else Printf.eprintf "[reply] empty ciaddr, ignoring\n"
    ;;

    let request (server_descriptor, socket_fd, client_addr, message)
      : (unit, string) result
      =
      let open Lib.Message in
      let open Lib.Server in
      let ( let* ) = Result.bind in
      (* fill the yiaddr *)
      let* message =
        if message.ciaddr = inet_addr_any
        then (
          let client_info_result =
            message.chaddr
            |> Mac.readable_of_bytes message.hlen
            |> Database.find_opt server_descriptor.db.clients
            |> Option.to_result ~none:"client not found"
          in
          let* ip_addr =
            match client_info_result with
            | Ok client_info -> inet_addr_of_string client_info.ip_addr |> Result.ok
            | Error err ->
              Printf.eprintf
                "[request] database lookup error: %s. Trying a system lookup instead...\n"
                err;
              Mac.lookup_ip_by_mac ~length_bytes:message.hlen ~mac_address:message.chaddr
              |> Option.to_result ~none:"client not found"
          in
          Result.ok { message with yiaddr = ip_addr })
        else Result.ok message
      in
      (* fill the file path *)
      let* message =
        let fname =
          if Lib.Utils.Str.is_empty message.file
          then Database.default_boot_file_name
          else message.file
        in
        let* boot_file_path =
          fname
          |> Database.find_opt server_descriptor.db.boot_files
          |> Option.to_result ~none:"boot file not found"
        in
        let full_file_path =
          Filename.concat server_descriptor.db.homedir boot_file_path
        in
        Result.ok { message with file = full_file_path }
      in
      (* fill the server address *)
      let* server_inet_addr =
        match getsockname socket_fd with
        | ADDR_INET (inet_addr, _) -> Result.ok inet_addr
        | _ -> Result.error "invalid socket type"
      in
      let message =
        if Lib.Utils.Inet.is_null message.siaddr
        then { message with siaddr = server_inet_addr }
        else message
      in
      (* mark the message as done *)
      let message = { message with op = BootOp.BOOTREPLY } in
      (* send the response *)
      let bytes = bytes_of_message message in
      let len_sent = sendto socket_fd bytes 0 (Bytes.length bytes) [] client_addr in
      Printf.eprintf "[request] sent back %d bytes\n" len_sent;
      Result.ok ()
    ;;
  end

  module RawMessageProcessing = struct
    open Lib.Message

    exception InvalidMessage of string

    (* Reject any non-viable messages based on length *)
    let filter_by_length ~raw_data () =
      let raw_data_length = Bytes.length raw_data in
      if raw_data_length < Lib.Message.min_message_length
         || raw_data_length > Lib.Message.max_message_length
      then
        InvalidMessage ("Invalid message length: " ^ string_of_int raw_data_length)
        |> raise
      else ()
    ;;

    (* Reject messages that prefer another server *)
    let filter_by_preferred_server ~server_descriptor ~message () =
      let open Lib.Server in
      String.iter (fun c -> Printf.eprintf "%d " (Char.code c)) server_descriptor.name;
      if message.op = BootOp.BOOTREQUEST
         && String.equal message.sname ""
         && (not @@ String.equal server_descriptor.name message.sname)
      then InvalidMessage "Not the preferred server" |> raise
      else ()
    ;;

    let try_parsing_exn ~raw_data () =
      try Lib.Message.message_of_bytes raw_data with
      | Invalid_argument msg ->
        InvalidMessage
          (Printf.sprintf
             "Failed to parse the message: '%s'; message: '%s'"
             msg
             (Bytes.to_string raw_data))
        |> raise
    ;;

    let parse_message ~server_descriptor ~raw_data () =
      try
        filter_by_length ~raw_data ();
        let message = try_parsing_exn ~raw_data () in
        filter_by_preferred_server ~server_descriptor ~message ();
        Result.Ok message
      with
      | InvalidMessage reason -> Result.Error reason
      | _ -> Result.Error "Unknown error"
    ;;
  end

  let on_connection ~socket_fd ~server_descriptor ~raw_data ~(client_addr : sockaddr) () =
    let open Lib.Message in
    match RawMessageProcessing.parse_message ~server_descriptor ~raw_data () with
    | Result.Error reason ->
      Printf.eprintf
        "[Acceptor] Invalid message: '%s'; message: '%s'\n"
        reason
        (Bytes.to_string raw_data)
    | Result.Ok message ->
      (match message.op with
       | BootOp.BOOTREQUEST ->
         Thread.create
           (fun a ->
             match ProtocolWorkers.request a with
             | Result.Ok () -> ()
             | Result.Error reason ->
               Printf.eprintf "[request] failed to handle the request: %s\n" reason;
               flush_all () |> ignore)
           (server_descriptor, socket_fd, client_addr, message)
       | BootOp.BOOTREPLY ->
         Thread.create
           ProtocolWorkers.reply
           (server_descriptor, socket_fd, client_addr, message))
      |> ignore
  ;;

  module ConnectionAcceptance = struct
    let accept_connections ~on_connection ~socket_fd () =
      let buffer = Bytes.make Lib.Message.max_message_length '\000' in
      let rec reception_loop () =
        Printf.eprintf "[main] awaiting connections...\n";
        flush Stdlib.stderr;
        let length_received, client_addr =
          recvfrom socket_fd buffer 0 (Bytes.length buffer) []
        in
        Printf.eprintf "[main] received %d bytes.\n" length_received;
        flush Stdlib.stderr;
        let raw_data = Bytes.sub buffer 0 length_received in
        (* Handle the message *)
        on_connection ~socket_fd ~raw_data ~client_addr ();
        (* Clear the input buffer *)
        Bytes.fill buffer 0 (Bytes.length buffer) '\000';
        (* Wait for another message *)
        reception_loop ()
      in
      reception_loop ()
    ;;
  end
end

module Initialisation = struct
  open Unix

  let initialise_socket () =
    let socket_fd = socket ~cloexec:true PF_INET SOCK_DGRAM 0 in
    setsockopt socket_fd SO_BROADCAST true;
    socket_fd
  ;;

  let initialise_address ~port () = ADDR_INET (inet_addr_any, port)

  let create_server ~port () =
    let socket_fd = initialise_socket () in
    bind socket_fd (initialise_address ~port ());
    socket_fd
  ;;

  let read_db ~db_path () = Lib.Server.Database.db_of_path db_path
end

module Cli = struct
  module Consts = struct
    let min_port = 1024
    let max_port = 65535
  end

  type t =
    { port : int
    ; db_path : string
    ; name : string
    }

  let port = ref 0
  let db_path = ref ""
  let name = ref ""
  let usage_message = "dune exec bin/server.exe -- -p <port> -d <database-file> [-n name]"

  let speclist =
    [ "-p", Arg.Set_int port, "Server port"
    ; "-d", Arg.Set_string db_path, "Databse file path"
    ; "-n", Arg.Set_string name, "Server name, no longer than 64 characters"
    ]
  ;;

  let print_usage () = Arg.usage speclist usage_message

  let parse_args () =
    let open Consts in
    let ( let* ) = Result.bind in
    Arg.parse speclist ignore usage_message;
    let* port =
      if !port >= min_port && !port <= max_port
      then Ok !port
      else
        Error
          (Printf.sprintf
             "Invalid port number '%d': expect a port between %d, and %d."
             !port
             min_port
             max_port)
    in
    let* db_path =
      if Sys.file_exists !db_path
      then Ok !db_path
      else
        Error
          (Printf.sprintf
             "Invalid database file path '%s': file does not exist."
             !db_path)
    in
    let* name =
      let name_length = String.length !name in
      if name_length <= 64
      then Ok !name
      else
        Error
          (Printf.sprintf
             "Invalid server name '%s': expected at most 64 characters, but found %d \
              instead."
             !name
             name_length)
    in
    Ok { port; db_path; name }
  ;;
end

let _ =
  match Cli.parse_args () with
  | Error err ->
    Printf.eprintf "%s\n" err;
    Cli.print_usage ();
    exit 1
  | Ok Cli.{ port; db_path; name } ->
    let server_descriptor =
      Lib.Server.{ name; db = Initialisation.read_db ~db_path () |> Result.get_ok; port }
    in
    ( port
    , server_descriptor
    , Initialisation.create_server
    , Loop.ConnectionAcceptance.accept_connections
    , Loop.on_connection )
    |> ignore;
    Printf.eprintf "port: %d\n" port;
    let socket_fd = Initialisation.create_server ~port () in
    Loop.ConnectionAcceptance.accept_connections
      ~on_connection:(Loop.on_connection ~server_descriptor)
      ~socket_fd
      ()
;;
