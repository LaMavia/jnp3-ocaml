let _ =
  let open Lib.Message in
  let file_path = Sys.argv.(1) in
  let file = open_out_bin file_path in
  let msg =
    { op = BootOp.BOOTREQUEST
    ; htype = 1
    ; hlen = 6
    ; hops = 1
    ; xid = Int32.of_int 12
    ; secs = 1
    ; ciaddr = Unix.inet_addr_any
    ; yiaddr = Unix.inet_addr_any
    ; siaddr = Unix.inet_addr_any
    ; giaddr = Unix.inet_addr_any
    ; chaddr = Mac.bytes_of_readable "02.60.8c.06.34.08"
    ; sname = String.make 64 '\000'
    ; file = "watch"
    ; vend = Bytes.make 64 '\000'
    }
  in
  let eq = if "a\000" = "a" then "yes" else "no" in
  Printf.eprintf "%s\n" eq;
  output_bytes file (bytes_of_message msg)
;;
