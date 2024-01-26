open Unix

let initialise_socket () =
  let socket_fd = socket ~cloexec:true PF_INET SOCK_DGRAM 0 in
  setsockopt socket_fd SO_BROADCAST true;
  socket_fd
;;

let initialise_address ~port () = ADDR_INET (inet_addr_any, port)

let create_server ~port () =
  let socket_fd = initialise_socket () in
  bind socket_fd (initialise_address ~port ());
  socket_fd
;;

let read_db ~db_path () = Lib.Server.Database.db_of_path db_path
