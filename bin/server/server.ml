let _ =
  match Cli.parse_args () with
  | Error err ->
    Printf.eprintf "%s\n" err;
    Cli.print_usage ();
    exit 1
  | Ok Cli.{ port; db_path; name } ->
    let server_descriptor =
      Lib.Server.{ name; db = Initialisation.read_db ~db_path () |> Result.get_ok; port }
    in
    ( port
    , server_descriptor
    , Initialisation.create_server
    , Loop.ConnectionAcceptance.accept_connections
    , Loop.on_connection )
    |> ignore;
    Printf.eprintf "port: %d\n" port;
    let socket_fd = Initialisation.create_server ~port () in
    Loop.ConnectionAcceptance.accept_connections
      ~on_connection:(Loop.on_connection ~server_descriptor)
      ~socket_fd
      ()
;;
