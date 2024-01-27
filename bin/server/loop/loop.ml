open Unix

module ConnectionAcceptance = struct
  let accept_connections = ConnectionAcceptance.accept_connections
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
         (fun a ->
           ProtocolWorkers.reply a;
           flush_all () |> ignore)
         (server_descriptor, socket_fd, client_addr, message))
    |> ignore
;;
