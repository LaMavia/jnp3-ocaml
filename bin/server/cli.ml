module Consts = struct
  let min_port = 1024
  let max_port = 65535
end

type t =
  { port : int
  ; db_path : string
  ; name : string
  }

let port = ref 0
let db_path = ref ""
let name = ref ""
let usage_message = "dune exec bin/server.exe -- -p <port> -d <database-file> [-n name]"

let speclist =
  [ "-p", Arg.Set_int port, "Server port"
  ; "-d", Arg.Set_string db_path, "Databse file path"
  ; "-n", Arg.Set_string name, "Server name, no longer than 64 characters"
  ]
;;

let print_usage () = Arg.usage speclist usage_message

let parse_args () =
  let open Consts in
  let ( let* ) = Result.bind in
  Arg.parse speclist ignore usage_message;
  let* port =
    if !port >= min_port && !port <= max_port
    then Ok !port
    else
      Error
        (Printf.sprintf
           "Invalid port number '%d': expect a port between %d, and %d."
           !port
           min_port
           max_port)
  in
  let* db_path =
    if Sys.file_exists !db_path
    then Ok !db_path
    else
      Error
        (Printf.sprintf "Invalid database file path '%s': file does not exist." !db_path)
  in
  let* name =
    let name_length = String.length !name in
    if name_length <= 64
    then Ok !name
    else
      Error
        (Printf.sprintf
           "Invalid server name '%s': expected at most 64 characters, but found %d \
            instead."
           !name
           name_length)
  in
  Ok { port; db_path; name }
;;
