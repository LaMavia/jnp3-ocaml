  let _ =
    if Array.length Sys.argv = 1 then ()
    else 
      let file_path = Sys.argv.(1) in
      let file = open_in_bin file_path in
      let rec read_chars chars =
        try read_chars (input_char file :: chars)
        with _ -> 
          chars 
          |> List.rev
          |> List.map (String.make 1)
          |> String.concat "" in 
      let contents = read_chars [] in
        close_in_noerr file;
      let decoded_message = 
        contents
        |> Bytes.of_string
        |> Lib.Message.message_of_bytes
      in Printf.printf "%s\n" 
          (Lib.Message.to_string decoded_message)
    
