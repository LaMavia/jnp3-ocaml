let () =
  let open Lib.Message in
  let open Unix in
    (* create a bytes buffer of size 300 filled with zeros *)
    let buffer = Bytes.make 300 '\000' in
    (* convert it into a bootp message *)
    let message = message_of_bytes buffer in
    (* set op field to bootrequest *)
    let message = { message with op = BootOp.BOOTREQUEST } in
    (* set htype field to ethernet *)
    let message = { message with htype = 1 } in
    (* set hlen field to 6 *)
    let message = { message with hlen = 6 } in
    (* set hops field to 0 *)
    let message = { message with hops = 0 } in
    (* set xid field to random *)
    let message = { message with xid = Random.int32 Int32.max_int } in
    (* set secs field to 0 *)
    let message = { message with secs = 0 } in
    (* set ciaddr field to 0 *)
    let message = { message with ciaddr = Unix.inet_addr_any } in
    (* set yiaddr field to 0 *)
    let message = { message with yiaddr = Unix.inet_addr_any } in
    (* set siaddr field to 0 *)
    let message = { message with siaddr = Unix.inet_addr_any } in
    (* set giaddr field to 0 *)
    let message = { message with giaddr = Unix.inet_addr_any } in
    (* set chaddr field to mac address gotten from file /sys/class/net/wlo1/address *)
    let message = { message with chaddr = Bytes.of_string "00:0c:29:4f:8e:5c" } in
    (* set sname field to 0 *)
    let message = { message with sname = String.make 64 '\000' } in
    (* set file field to 0 *)
    let message = { message with file = String.make 128 '\000' } in
    (* set vend field to 0 *)
    let message = { message with vend = Bytes.make 64 '\000' } in
      (* convert the message back to bytes *)
      let buffer = bytes_of_message message in
        let server_port = 8080 in
        let client_port = 8081 in
        let broadcast_address = Unix.inet_addr_of_string "255.255.255.255" in
          (* create a socket *)
          let socket = socket PF_INET SOCK_DGRAM 0 in
          (* set the socket to broadcast *)
          setsockopt socket SO_BROADCAST true;
          (* bind the socket to the client port *)
          bind socket (ADDR_INET (Unix.inet_addr_any, client_port));
          (* send the message to the broadcast address on the server port *)
          sendto socket buffer 0 (Bytes.length buffer) [] (ADDR_INET (broadcast_address, server_port)) |> ignore;
          
      