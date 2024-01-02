(* let accept_connection_main ~socket_fd () = undefined *)

module Loop = struct
  open Unix

  module ProtocolWorkers = struct
    let reply (socket_fd, client_addr, message) = 
      (socket_fd, message, client_addr) |> ignore 

    let request (server_descriptor, socket_fd, client_addr, message): (unit, unit) result =       
      let open Lib.Message in
      let open Lib.Server in 

      let ( let* ) = Result.bind in
      (message, client_addr) |> ignore;

      (* fill the yiaddr *)
      let* message = 
        if message.ciaddr = inet_addr_any then 
          let* client_info = message.chaddr
            |> Bytes.to_string  
            |> Hashtbl.find_opt server_descriptor.db.clients
            |> Option.to_result ~none:() 
          in Result.ok { message with yiaddr = inet_addr_of_string client_info.ip_addr } 
        else Result.ok message in

      (* fill the file path *)
      let* message =
        let fname = 
          if message.file = "" then 
            Database.default_boot_file_name 
          else message.file in
        let* boot_file_path = fname
          |> Hashtbl.find_opt server_descriptor.db.boot_files
          |> Option.to_result ~none:() in
        let full_file_path = 
          Filename.concat 
            server_descriptor.db.homedir 
            boot_file_path 
        in Result.ok { message with file = full_file_path } in 
      
      (* mark the message as done *)
      let* server_inet_addr = 
        match getsockname socket_fd with
        | ADDR_INET (inet_addr, _) -> Result.ok inet_addr
        | _ -> Result.error ()
        in
      let message = { message with siaddr = server_inet_addr; op = BootOp.BOOTREPLY } in
      (* send the response *)
      let* bytes = Result.ok (bytes_of_message message) in
        assert (sendto socket_fd bytes 0 (Bytes.length bytes) [] client_addr != 1);
        Result.ok ()
  end

  module RawMessageProcessing = struct
    open Lib.Message

    exception InvalidMessage of string

    (* Reject any non-viable messages based on length *)
    let filter_by_length ~raw_data () =      
      let raw_data_length = Bytes.length raw_data in
      if raw_data_length < Lib.Message.min_message_length ||
         raw_data_length > Lib.Message.max_message_length 
        then InvalidMessage ("Invalid message length: " ^ string_of_int raw_data_length) |> raise
        else ()

    (* Reject messages that prefer another server *)
    let filter_by_preferred_server ~server_descriptor ~message () =
      let open Lib.Server in
      if message.op = BootOp.BOOTREQUEST 
        && message.sname != "" 
        && server_descriptor.name != message.sname 
        then InvalidMessage "Not the preferred server" |> raise
        else ()

    let try_parsing_exn ~raw_data () =
      try Lib.Message.message_of_bytes raw_data
      with 
        | Invalid_argument msg -> InvalidMessage (
            Printf.sprintf 
              "Failed to parse the message: '%s'; message: '%s'" 
              msg
              (Bytes.to_string raw_data)
          ) |> raise
        
    let parse_message ~server_descriptor ~raw_data () =
      try begin
        filter_by_length ~raw_data ();
        let message = try_parsing_exn ~raw_data () in
        filter_by_preferred_server ~server_descriptor ~message ();
        Result.Ok message
      end with 
        | InvalidMessage reason -> Result.Error reason
        | _                     -> Result.Error "Unknown error"
  end

  let on_connection ~socket_fd ~server_descriptor ~raw_data ~(client_addr:sockaddr) () = 
    let open Lib.Message in
    match 
      RawMessageProcessing.parse_message 
        ~server_descriptor 
        ~raw_data
        ()
    with 
    | Result.Error reason -> 
        Printf.eprintf 
          "[Acceptor] Invalid message: '%s'; message: '%s'" 
          reason
          (Bytes.to_string raw_data)
    | Result.Ok message ->
        (match message.op with
        | BootOp.BOOTREQUEST -> 
          Thread.create 
            ProtocolWorkers.request
            (server_descriptor, socket_fd, client_addr, message)
        | BootOp.BOOTREPLY -> 
          Thread.create 
            ProtocolWorkers.reply
            (socket_fd, client_addr, message)
        ) |> ignore

  module ConnectionAcceptance = struct
    let accept_connections ~on_connection ~socket_fd () =
      let buffer = Bytes.make Lib.Message.max_message_length '\000' in
      let rec reception_loop () =
        let (length_received, client_addr) = 
          recvfrom 
            socket_fd 
            buffer 0 (Bytes.length buffer) 
            [] 
          in
        let raw_data = Bytes.sub buffer 0 length_received in
        (* Handle the message *)
        on_connection ~socket_fd ~raw_data ~client_addr ();
        (* Clear the input buffer *)
        Bytes.fill buffer 0 (Bytes.length buffer) '\000';
        (* Wait for another message *)
        reception_loop ()

      in reception_loop ()
  end
end

module Initialisation = struct 
  open Unix

  let initialise_socket () =
    socket ~cloexec:true PF_INET SOCK_DGRAM 0
 
  let initialise_address ~port () =
    ADDR_INET (inet_addr_any, port)

  let create_server ~port () =
    let socket_fd = initialise_socket () in
    bind socket_fd (initialise_address ~port:port ());
    socket_fd

  let read_db ~db_path () =
    Lib.Server.Database.db_of_path db_path
end


let _ = 
  let port = 0 in
  let db_path = "" in
  let server_descriptor = Lib.Server.{
    name = "";
    db = Initialisation.read_db ~db_path () |> Result.get_ok
  } in
  let socket_fd = Initialisation.create_server ~port:port () in
  Loop.ConnectionAcceptance.accept_connections 
    ~on_connection:(Loop.on_connection ~server_descriptor)
    ~socket_fd
    () 
