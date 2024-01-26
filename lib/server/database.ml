module StringHashtbl = Hashtbl.Make (struct
    type t = string

    let equal = String.equal
    let hash = Hashtbl.hash
  end)

type t =
  { homedir : string
  ; (* generic_name -> path *)
    boot_files : string StringHashtbl.t
  ; (* hardware_addr -> client_infp *)
    clients : client_info StringHashtbl.t
  }

and client_info =
  { hostname : string
  ; hardware_type : string
  ; hardware_addr : string
  ; ip_addr : string
  ; generic_name : string option
  ; suffix : string option
  }

let string_of_client_info info =
  Printf.sprintf
    "\n\
     {  hostname      = %s\n\
    \   hardware_type = %s\n\
    \   hardware_addr = %s\n\
    \   ip_addr       = %s\n\
    \   generic_name  = %s\n\
    \   suffix        = %s\n\
     }"
    info.hostname
    info.hardware_type
    info.hardware_addr
    info.ip_addr
    (Option.value ~default:"[null]" info.generic_name)
    (Option.value ~default:"[null]" info.suffix)
;;

let string_of_hash tbl string_of_key string_of_val =
  StringHashtbl.to_seq tbl
  |> Seq.fold_left
       (fun u (k, v) -> u ^ string_of_key k ^ " = " ^ string_of_val v ^ "\n")
       ""
  |> fun s -> "{" ^ s ^ "}"
;;

let to_string db =
  Printf.sprintf
    "{ homedir = %s\n   boot_files =\n%s\n   clients =\n%s\n}"
    db.homedir
    (string_of_hash db.boot_files Fun.id Fun.id)
    (string_of_hash db.clients Fun.id string_of_client_info)
;;

let can_be_ignored line =
  let trimmed_line = String.trim line in
  trimmed_line = ""
  || String.starts_with ~prefix:"#" trimmed_line
  || String.starts_with ~prefix:"%" trimmed_line
;;

let split_by_whitespace line =
  (* as per String.trim docs *)
  let while_space_chars = [ ' '; '\x0C'; '\n'; '\r'; '\t' ] in
  List.fold_left
    (fun lines delimiter -> List.concat_map (String.split_on_char delimiter) lines)
    [ line ]
    while_space_chars
  |> List.map String.trim
  |> List.filter (fun s -> String.length s != 0)
;;

(* Parse the given line into either a boot file entry (Left)
   or a client_info entry (Right).
   Returns a pair (key, value) for either type.

   Raises `Invalid_argument` if the line doesn't parse
   into either entry type.
*)
let db_entry_of_line line =
  let open Either in
  match split_by_whitespace line with
  (* boot_file entry *)
  | [ generic_name; path ] -> Left (generic_name, path)
  | hostname :: hardware_type :: hardware_addr :: ip_addr :: optional_fields ->
    let generic_name = List.nth_opt optional_fields 0 in
    let suffix = List.nth_opt optional_fields 1 in
    Right
      ( hardware_addr
      , { hostname; hardware_type; hardware_addr; ip_addr; generic_name; suffix } )
  | [] -> invalid_arg "No fragments; expected the line to be non-empty."
  | f :: fs ->
    let expl = List.fold_left (fun acc fragment -> acc ^ "|" ^ fragment) f fs in
    Printf.eprintf "%s" expl;
    invalid_arg expl
;;

let parse_database_file lines =
  try
    let valid_lines = List.filter (Fun.negate can_be_ignored) lines in
    let trimmed_lines = List.map String.trim valid_lines in
    match trimmed_lines with
    | [] -> Result.Error ()
    | homedir :: other_lines ->
      let boot_file_entries, client_entries =
        List.partition_map db_entry_of_line other_lines
      in
      let boot_file_table = boot_file_entries |> List.to_seq |> StringHashtbl.of_seq in
      let client_table = client_entries |> List.to_seq |> StringHashtbl.of_seq in
      Result.ok { homedir; boot_files = boot_file_table; clients = client_table }
  with
  | _ ->
    Result.ok
      { homedir = ""
      ; boot_files = StringHashtbl.create 0
      ; clients = StringHashtbl.create 0
      }
;;

let db_of_path db_file_path =
  let file_in_channel = open_in db_file_path in
  let rec read_lines lines =
    try
      let line = input_line file_in_channel in
      read_lines (line :: lines)
    with
    | _ -> List.rev lines
  in
  let res = read_lines [] |> parse_database_file in
  res
;;
