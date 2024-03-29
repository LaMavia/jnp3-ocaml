let _ =
  let open Lib.Message in
  let file_path = Sys.argv.(1) in
  let file = open_out_bin file_path in
  let msg =
    { op = BootOp.BOOTREPLY
    ; htype = 1
    ; hlen = 6
    ; hops = 1
    ; xid = Int32.of_int 12
    ; secs = 1
    ; ciaddr = Unix.inet_addr_of_string "36.19.0.5"
    ; yiaddr = Unix.inet_addr_any
    ; siaddr = Unix.inet_addr_any
    ; giaddr = Unix.inet_addr_any
    ; chaddr = Mac.bytes_of_readable "02.60.8c.06.34.98"
    ; sname = String.make 64 '\000'
    ; file = "gate"
    ; vend = Bytes.make 64 '\000'
    }
  in
  output_bytes file (bytes_of_message msg)
;;
