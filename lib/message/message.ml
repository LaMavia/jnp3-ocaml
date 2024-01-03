module BootOp = Op
module GenericInt = GenericInt
module Mac = Mac

type t = {
  op    : Op.t          ;
  htype : int           ;
  hlen  : int           ;
  hops  : int           ;
  xid   : Int32.t       ;
  secs  : int           ;
  ciaddr: Unix.inet_addr;
  yiaddr: Unix.inet_addr;
  siaddr: Unix.inet_addr;
  giaddr: Unix.inet_addr;
  chaddr: bytes         ;
  sname : string        ;
  file  : string        ;
  vend  : bytes         ;
}

let to_string msg =
  Printf.sprintf 
"{ op    = %s 
  htype  = %d
  hlen   = %d
  hops   = %d
  xid    = %s
  secs   = %d
  ciaddr = %s
  yiaddr = %s
  siaddr = %s
  giaddr = %s
  chaddr = %s
  sname  = %s
  file   = %s
  vend   = %s
}"
    (Op.to_string msg.op)
    msg.htype
    msg.hlen
    msg.hops
    (Int32.to_string msg.xid)
    msg.secs
    (Unix.string_of_inet_addr msg.ciaddr)
    (Unix.string_of_inet_addr msg.yiaddr)
    (Unix.string_of_inet_addr msg.siaddr)
    (Unix.string_of_inet_addr msg.giaddr)
    (Mac.readable_of_bytes msg.hlen msg.chaddr)
    msg.sname
    msg.file
    (Bytes.to_string msg.vend)

let min_message_length = 236
let max_message_length = min_message_length + 64

let message_of_bytes bytes = {
  op = Bytes.sub bytes 0 1 
    |> Op.boot_op_of_bytes 
    |> Option.get;
  htype = Bytes.sub bytes 1 1 
    |> GenericInt.t_of_bytes
    |> GenericInt.int_of_t;
  hlen = Bytes.sub bytes 2 1 
    |> GenericInt.t_of_bytes
    |> GenericInt.int_of_t;
  hops = Bytes.sub bytes 3 1 
    |> GenericInt.t_of_bytes
    |> GenericInt.int_of_t;
  xid = Bytes.sub bytes 4 4
    |> GenericInt.t_of_bytes
    |> GenericInt.int32_of_t;
  secs = Bytes.sub bytes 8 2 
    |> GenericInt.t_of_bytes
    |> GenericInt.int_of_t;
  ciaddr = Bytes.sub bytes 12 4
    |> Address.addr_of_bytes;
  yiaddr = Bytes.sub bytes 16 4
    |> Address.addr_of_bytes;
  siaddr = Bytes.sub bytes 20 4
    |> Address.addr_of_bytes;
  giaddr = Bytes.sub bytes 24 4
    |> Address.addr_of_bytes;
  chaddr = Bytes.sub bytes 28 16;
  sname = Bytes.sub bytes 44 64
    |> Bytes.to_string;
  file = Bytes.sub bytes 108 128
    |> Bytes.to_string;
  vend = 
    let length = Bytes.length bytes in
    if length > min_message_length
      then Bytes.sub bytes min_message_length (length - min_message_length) 
      else Bytes.empty
    ;
}

let bytes_of_message message = 
  let open Bytes in
  let output_buffer = make (236 + length message.vend) '\000' in
  blit (message.op |> Op.bytes_of_boot_op)            0 output_buffer 0   1; 
  blit GenericInt.(`int8 message.htype |> bytes_of_t) 0 output_buffer 1   1; 
  blit GenericInt.(`int8 message.hlen  |> bytes_of_t) 0 output_buffer 2   1; 
  blit GenericInt.(`int8 message.hops  |> bytes_of_t) 0 output_buffer 3   1;  
  blit GenericInt.(`int32 message.xid  |> bytes_of_t) 0 output_buffer 4   4;  
  blit GenericInt.(`int16 message.secs |> bytes_of_t) 0 output_buffer 8   2; 
  blit (Address.bytes_of_addr message.ciaddr)         0 output_buffer 12  4; 
  blit (Address.bytes_of_addr message.yiaddr)         0 output_buffer 16  4; 
  blit (Address.bytes_of_addr message.siaddr)         0 output_buffer 20  4; 
  blit (Address.bytes_of_addr message.giaddr)         0 output_buffer 24  4; 
  blit message.chaddr                                 0 output_buffer 28  16; 
  blit (message.sname |> of_string)                   0 output_buffer 44  (min (String.length message.sname) 64 );
  blit (message.file  |> of_string)                   0 output_buffer 108 (min (String.length message.file ) 128);
  blit message.vend                                   0 output_buffer 236 (min (length message.vend) 64 );
  output_buffer
