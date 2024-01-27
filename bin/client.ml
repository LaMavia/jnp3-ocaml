module Client = struct
  open Unix
  open Lib.Message

  let rec await_response socket server_address_with_port message timeout =
    let buffer = Bytes.make 300 '\000' in
    try
      Unix.sleep timeout;
      recvfrom socket buffer 0 (Bytes.length buffer) [] |> ignore;
      let received_message = message_of_bytes buffer in
      if received_message.xid = message.xid
      then
        begin
          Printf.eprintf "received message with matching xid\n";
        end
      else
        Printf.eprintf "received message with non matching xid\n";
    with
    | Unix_error (EAGAIN, _, _) ->
      Printf.eprintf "errno = EAGAIN\n";
      let buffer = bytes_of_message message in
      sendto
        socket
        buffer
        0
        (Bytes.length buffer)
        []
        server_address_with_port
      |> ignore;
      Printf.eprintf "sent\n";
      let message = { message with secs = 2 * timeout - 1 } in
      await_response socket server_address_with_port message (timeout * 2)
  ;;
end

let _ =
  let usage_msg = "usage: " ^ Sys.argv.(0) ^ " --server-port <server_port> --client-port <client_port> [--server-addr <server_address_if_known>] [--src-addr <your_address_if_known>] [--server-name <server_host_name_if_known>] [--file-name <requested_file_name>] [--mac-address <mac_address_to_find>]" in

  let server_port = ref 0 in
  let client_port = ref 0 in
  let string_server_address = ref "255.255.255.255" in
  let string_client_address = ref "" in
  let string_server_name = ref "" in
  let string_file_name = ref "" in
  let string_mac_address = ref "02.60.8c.06.34.98" in
  

  let speclist =
    [
      ("--server-port", Arg.Set_int server_port, "Server port");
      ("--client-port", Arg.Set_int client_port, "Client port");
      ("--server-addr", Arg.Set_string string_server_address, "Server address if known");
      ("--src-addr", Arg.Set_string string_client_address, "Your address if known");
      ("--server-name", Arg.Set_string string_server_name, "Server host name if known");
      ("--file-name", Arg.Set_string string_file_name, "Requested file name");
      ("--mac-address", Arg.Set_string string_mac_address, "MAC address to find")
    ] in

  Arg.parse speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage_msg;
  if !server_port = 0 || !client_port = 0
  then
    begin
      Arg.usage speclist usage_msg;
      exit 1
    end;

  Out_channel.set_buffered stderr false;
  Printf.eprintf "starting\n";
  let open Lib.Message in
  let open Unix in
  let buffer = Bytes.of_string (String.make 1 '\001' ^ String.make 299 '\000') in
  let message = message_of_bytes buffer in
  let message = { message with op = BootOp.BOOTREQUEST } in
  let message = { message with htype = 1 } in
  let message = { message with hlen = 6 } in
  let message = { message with hops = 0 } in
  let message = { message with xid = Random.int32 Int32.max_int } in
  let message = { message with secs = 0 } in
  let message = { message with ciaddr = match !string_client_address with
    | "" -> Unix.inet_addr_any
    | _ -> Unix.inet_addr_of_string !string_client_address
  } in
  let message = { message with yiaddr = Unix.inet_addr_any } in
  let message = { message with siaddr = Unix.inet_addr_any } in
  let message = { message with giaddr = Unix.inet_addr_any } in
  let message = { message with chaddr = Mac.bytes_of_readable !string_mac_address } in
  let message = { message with sname = match !string_server_name with
    | "" -> String.make 64 '\000'
    | _ -> !string_server_name
  } in
  let message = { message with file = match !string_file_name with
    | "" -> String.make 128 '\000'
    | _ -> !string_file_name
  } in
  let message = { message with vend = Bytes.make 64 '\000' } in
  let buffer = bytes_of_message message in
  let start_timeout = 1 in
  let server_address = Unix.inet_addr_of_string !string_server_address in
  let server_address_with_port = ADDR_INET (server_address, !server_port) in
  let socket = socket PF_INET SOCK_DGRAM 0 in
  setsockopt socket SO_BROADCAST true;
  set_nonblock socket;
  bind socket (ADDR_INET (Unix.inet_addr_any, !client_port));
  sendto
    socket
    buffer
    0
    (Bytes.length buffer)
    []
    server_address_with_port
  |> ignore;
  Printf.eprintf "sent\n";
  Client.await_response socket server_address_with_port message start_timeout
;;
