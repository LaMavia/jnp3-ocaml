let addr_of_bytes bytes = 
  Bytes.to_string bytes 
  |> Unix.inet_addr_of_string

let bytes_of_addr addr =
  Unix.string_of_inet_addr addr
  |> Bytes.of_string 

