type t =
  | BOOTREQUEST
  | BOOTREPLY

let bytes_of_boot_op op =
  match op with
  | BOOTREQUEST -> Bytes.make 1 (Char.chr 1)
  | BOOTREPLY -> Bytes.make 1 (Char.chr 2)
;;

let boot_op_of_bytes bytes =
  if bytes = Bytes.empty
  then None
  else (
    match Bytes.get bytes 0 |> Char.code with
    | 1 -> Some BOOTREQUEST
    | 2 -> Some BOOTREPLY
    | _ -> None)
;;

let to_string = function
  | BOOTREQUEST -> "BOOTREQUEST"
  | BOOTREPLY -> "BOOTREPLY"
;;
