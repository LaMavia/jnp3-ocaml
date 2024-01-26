open Lib.Message

exception InvalidMessage of string

(* Reject any non-viable messages based on length *)
let filter_by_length ~raw_data () =
  let raw_data_length = Bytes.length raw_data in
  if raw_data_length < Lib.Message.min_message_length
     || raw_data_length > Lib.Message.max_message_length
  then
    InvalidMessage ("Invalid message length: " ^ string_of_int raw_data_length) |> raise
  else ()
;;

(* Reject messages that prefer another server *)
let filter_by_preferred_server ~server_descriptor ~message () =
  let open Lib.Server in
  String.iter (fun c -> Printf.eprintf "%d " (Char.code c)) server_descriptor.name;
  if message.op = BootOp.BOOTREQUEST
     && String.equal message.sname ""
     && (not @@ String.equal server_descriptor.name message.sname)
  then InvalidMessage "Not the preferred server" |> raise
  else ()
;;

let try_parsing_exn ~raw_data () =
  try Lib.Message.message_of_bytes raw_data with
  | Invalid_argument msg ->
    InvalidMessage
      (Printf.sprintf
         "Failed to parse the message: '%s'; message: '%s'"
         msg
         (Bytes.to_string raw_data))
    |> raise
;;

let parse_message ~server_descriptor ~raw_data () =
  try
    filter_by_length ~raw_data ();
    let message = try_parsing_exn ~raw_data () in
    filter_by_preferred_server ~server_descriptor ~message ();
    Result.Ok message
  with
  | InvalidMessage reason -> Result.Error reason
  | _ -> Result.Error "Unknown error"
;;
