module Database = struct
  type t = Database.t
  type client_info = Database.client_info

  let db_of_path = Database.db_of_path
  let default_boot_file_name = "default"
  let to_string = Database.to_string
end

type server_descriptor =
  { name : string
  ; db : Database.t
  }
