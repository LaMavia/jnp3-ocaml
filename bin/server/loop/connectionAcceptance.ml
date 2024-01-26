open Unix

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
