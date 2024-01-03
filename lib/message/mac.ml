let lookup_ip_by_mac ~mac_address =
  (* https://superuser.com/questions/894424/is-there-an-easy-way-to-do-a-host-lookup-by-mac-address-on-a-lan *)
  let stringified_mac_address = 
    Bytes.to_string mac_address 
    |> Filename.quote in
  let command = Printf.sprintf "arp -n | grep '%s' | awk '{print $1}'" stringified_mac_address in
  (* https://www.ocamlwiki.com/wiki/OCaml_Shell_Command_Execution *)
  let command_output = Unix.open_process_in command in
  try  
    let addr = 
      input_line command_output 
      |> Unix.inet_addr_of_string 
    in Unix.close_process_in command_output |> ignore; 
       Some addr 
  with _ -> None

let readable_of_bytes length_bytes bytes =
  let addr_bytes = Bytes.sub bytes 0 length_bytes in
    Seq.unfold
      (fun i -> 
        if i < length_bytes then
          Option.some (Bytes.get_uint8 addr_bytes i, i+1)
        else Option.none
      ) 0
    |> Seq.map (Printf.sprintf "%02x")
    |> List.of_seq
    |> String.concat "."

let bytes_of_readable mac_address =
  (* 02.60.8c.06.34.98 *)
  let bytes = Bytes.make 16 '\000' in
  let fragments = String.split_on_char '.' mac_address in
  fragments
  |> List.map (fun f -> Scanf.sscanf f "%x" Fun.id) 
  |> List.iteri (Bytes.set_uint8 bytes);
  bytes

  
