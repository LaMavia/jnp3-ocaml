open Unix

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

let request (server_descriptor, socket_fd, client_addr, message) : (unit, string) result =
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
      match Database.find_opt server_descriptor.db.boot_files fname with
      | Some file_path ->
        Result.ok @@ Filename.concat server_descriptor.db.homedir file_path
      | None when fname = Database.default_boot_file_name -> Result.ok ""
      | None -> Result.error "boot file not found"
    in
    Result.ok { message with file = boot_file_path }
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
