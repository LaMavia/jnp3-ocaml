let addr_of_bytes bytes = 
  let segments = 
    [0; 1; 2; 3] 
    |> List.map (fun i -> Bytes.get_uint8 bytes i) 
    |> List.map string_of_int in
  let str = match segments with
    | a::b::c::d::[] -> Printf.sprintf "%s.%s.%s.%s" a b c d 
    | _ -> assert false
  in str |> Unix.inet_addr_of_string

let bytes_of_addr addr =
  let bytes = Bytes.create 4 in
  let segments = 
    Unix.string_of_inet_addr addr
    |> String.split_on_char '.'
  in 
    List.iteri 
      (fun i s -> Bytes.set_int8 bytes i (int_of_string s))
      segments;
    bytes
  

