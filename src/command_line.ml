(* Command line *)

open Lwt
open Read_file
open Signature
open Cmdliner
open Network_interaction_close_bound
open Camltc


let host = "127.0.0.1"
let port = 9000
let soc = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0         (* Server socket *)
let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port)   (* Address *)

let hash_function = function l ->
  Sha1.to_hex (Sha1.string l)


(* Loading the database *)
let load file db_name partition block_size=
  Lwt_main.run
    (
      let partition_function =
        match partition with
        | "words" -> words
        | "blocks" -> blocks ~size:block_size
        | "whitespace" -> blocks_using_whitespace ~size:block_size
        | "lines" -> lines
        | _ -> blocks ~size:block_size
      in
      partition_function file hash_function >>= fun parts ->
      Signature.database db_name >>= fun db ->
      Signature.commit_info parts db >>= fun _ ->
      Lwt.return ()
    )


let load_database_cmd =
  let file =
    let doc = "Initial file to load in database." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)
  in
  let db_name =
    let doc = "Name of the database." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"DB_NAME" ~doc)
  in
  let partition =
    let doc = "Partition function to use." in
    Arg.(required & pos 2 (some string) None & info [] ~docv:"PARTS" ~doc)
  in
  let block_size =
    let doc = "Size of the blocks." in
    Arg.(value & opt int 4096 & info ["s"; "size"] ~docv:"SIZE" ~doc)
  in
  let doc = "Loading database." in
  let man = [
    `S "DESCRIPTION" ;
    `P "Partition a file and initialize database with the obtained parts"]
  in
  Term.(pure load $ file $ db_name $ partition $ block_size),
  Term.info "load_db" ~doc ~man


(* Server *)
let server db =
  Lwt_main.run
    (
      let module N = Sync_with_network in
      Hotc.create db [] >>= fun db ->
      N.server soc addr db hash_function
    )


let server_cmd =
  let db =
    let doc = "Database to use in syncing." in
    Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv:"DB_NAME" ~doc)
  in
  let doc = "Setting up the server." in
  let man = [
    `S "DESCRIPTION" ;
    `P "Sets up the server, with a specified database at its disposal."]
  in
  Term.(pure server $ db),
  Term.info "server" ~doc ~man


(* Client *)
let client file dest partition block_size=
  Lwt_main.run
    (
      let module N = Sync_with_network in
      let partition_function =
        match partition with
        | "words" -> words
        | "blocks" -> blocks ~size:block_size
        | "whitespace" -> blocks_using_whitespace ~size:block_size
        | "lines" -> lines
        | _ -> blocks ~size:block_size
      in
      N.sync_with_server addr file partition_function hash_function dest
    )

let client_cmd =
  let file =
    let doc = "File to sync." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)
  in
  let dest =
    let doc = "Destination file." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"DEST" ~doc)
  in
  let partition =
    let doc = "Partition function to use." in
    Arg.(required & pos 2 (some string) None & info [] ~docv:"PARTS" ~doc)
  in
  let block_size =
    let doc = "Size of the blocks." in
    Arg.(value & opt int 4096 & info ["s"; "size"] ~docv:"SIZE" ~doc)
  in
  let doc = "Setting up the client." in
  let man = [
    `S "DESCRIPTION" ;
    `P "Sets up the client. A file to reconstruct at a specified location is provided."]
  in
  Term.(pure client $ file $ dest $ partition $ block_size),
  Term.info "client" ~doc ~man


(* Default *)
let default_cmd =
  let doc = "tests for syncing" in
  let man = [
    `S "DESCRIPTION" ;
    `P "syncing. Client-server setup" ]
  in
  Term.(ret (pure (`Ok ()) )) ,
  Term.info "default" ~doc ~man



(* Main *)
let cmds = [load_database_cmd ; server_cmd ; client_cmd ]

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0