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

