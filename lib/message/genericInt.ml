type t = [`int8 of int | `int16 of int | `int32 of Int32.t | `int64 of Int64.t]

let int_of_t = function
  | `int8  n -> n
  | `int16 n -> n
  | _        -> assert false

let int32_of_t = function
  | `int32 n -> n
  | _        -> assert false

let int64_of_t = function
  | `int64 n -> n
  | _        -> assert false

let length = function
  | `int8  _ -> 1
  | `int16 _ -> 2
  | `int32 _ -> 4
  | `int64 _ -> 8

let bytes_of_t num =
  let bytes = length num |> Bytes.create in
  (match num with
  | `int8  n -> Bytes.set_int8     bytes 0 n
  | `int16 n -> Bytes.set_int16_be bytes 0 n
  | `int32 n -> Bytes.set_int32_be bytes 0 n
  | `int64 n -> Bytes.set_int64_be bytes 0 n
  ); bytes

let t_of_bytes bytes = 
  match Bytes.length bytes with
  | 1 -> `int8 (Bytes.get_int8 bytes 0)
  | 2 -> `int16 (Bytes.get_int16_be bytes 0)
  | 4 -> `int32 (Bytes.get_int32_be bytes 0)
  | 8 -> `int64 (Bytes.get_int64_be bytes 0)
  | n -> failwith @@ "invalid number length: " ^ string_of_int n 

