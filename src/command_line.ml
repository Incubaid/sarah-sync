(* This module provides the possibility to use the command line*)

open Lwt
open Read_file
open Signature
open Cmdliner
open Camltc


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
    let doc = "Size of the blocks. Only needs to be specified when the partition function requires it." in
    Arg.(value & opt int 4096 & info ["s"; "size"] ~docv:"SIZE" ~doc)
  in
  let doc = "Loading database." in
  let man = [
    `S "DESCRIPTION" ;
    `P "Partition a file and initialize database with the obtained parts."]
  in
  Term.(pure load $ file $ db_name $ partition $ block_size),
  Term.info "load_db" ~doc ~man


(* Server *)
let server db host port field_size =
  Lwt_main.run
    (
      let soc = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in           (* Server socket *)
      let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in     (* Address *)
      match field_size with
      | "auto" ->
        let module N = Network_interaction_close_bound.Sync_with_network in
        Hotc.create db [] >>= fun db ->
        N.server soc addr db hash_function
      | s ->
        let f_w = int_of_string s in
        let module F = FiniteField.Make(struct
          let w = f_w
        end)
        in
        let module N = Network_interaction.Sync_with_network(F) in
        Hotc.create db [] >>= fun db ->
        N.server soc addr db hash_function
    )


let server_cmd =
  let db =
    let doc = "Database to use in syncing. The database contains the information available on the server." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"DB_NAME" ~doc)
  in
  let host =
    let doc = "IP-address of host." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"HOST" ~doc)
  in
  let port =
    let doc = "Port to use." in
    Arg.(required & pos 2 (some int) None & info [] ~docv:"PORT" ~doc)
  in
  let field_size =
    let doc = "Size w of the finite field GF(2^w)." in
    Arg.(value & opt string "auto" & info ["f"; "field"] ~docv:"FIELD" ~doc)
  in
  let doc = "Setting up the server." in
  let man = [
    `S "DESCRIPTION" ;
    `P "Sets up the server, with a specified database at its disposal."]
  in
  Term.(pure server $ db $ host $ port $ field_size ),
  Term.info "server" ~doc ~man


(* Client *)
let client file dest host port partition field_size block_size=
  Time.time Lwt_main.run
    (
      let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in     (* Address *)
      let partition_function =
        match partition with
        | "words" -> words
        | "blocks" -> blocks ~size:block_size
        | "whitespace" -> blocks_using_whitespace ~size:block_size
        | "lines" -> lines
        | _ -> blocks ~size:block_size
      in
      match field_size with
      | "auto" ->
        let module N = Network_interaction_close_bound.Sync_with_network in
        N.sync_with_server addr file partition_function hash_function dest
      | s ->
              let f_w = int_of_string s in
              let module F = FiniteField.Make(struct
                let w = f_w
              end)
              in
              let module N = Network_interaction.Sync_with_network(F) in
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
  let host =
    let doc = "IP-address of host." in
    Arg.(required & pos 2 (some string) None & info [] ~docv:"HOST" ~doc)
  in
  let port =
    let doc = "Port to use." in
    Arg.(required & pos 3 (some int) None & info [] ~docv:"PORT" ~doc)
  in
  let partition =
    let doc = "Partition function to use." in
    Arg.(required & pos 4 (some string) None & info [] ~docv:"PARTS" ~doc)
  in
  let field_size =
    let doc = "Size w of the finite field GF(2^w)." in
    Arg.(value & opt string "auto" & info ["f"; "field"] ~docv:"FIELD" ~doc)
  in
  let block_size =
    let doc = "Size of the blocks. Only needs to be specified when the partition function requires it." in
    Arg.(value & opt int 4096 & info ["s"; "size"] ~docv:"SIZE" ~doc)
  in
  let doc = "Setting up the client." in
  let man = [
    `S "DESCRIPTION" ;
    `P "Sets up the client. A file to reconstruct at a specified location is provided."]
  in
  Term.(pure client $ file $ dest $ host $ port $ partition $ field_size $ block_size),
  Term.info "client" ~doc ~man


(* Local (no network) *)
let local file_1 file_2 dest partition field_size block_size db_name =
  match db_name with
  | "none" ->   (* No database will be used in the syncing *)
    begin
      match field_size with
      | "auto" ->
        begin
          let module Sync = Sync_close_bound_no_db.Syncing in
          let f =
            match partition with
            | "words" -> Sync.sync_with_words file_1 file_2 hash_function
            | "blocks" -> Sync.sync_with_blocks file_1 file_2 block_size hash_function
            | "whitespace" -> Sync.sync_with_whitespace file_1 file_2 block_size hash_function
            | "lines" -> Sync.sync_with_lines file_1 file_2 hash_function
            | _ -> Sync.sync_with_blocks file_1 file_2 block_size hash_function
          in
          Time.time f dest
        end
      | s ->
        begin
          let f_w = int_of_string s in
          let module F = FiniteField.Make(struct
            let w = f_w
          end)
          in
          let module Sync = Sync_two_files_no_db.Syncing(F) in
          let f =
            match partition with
            | "words" -> Sync.sync_with_words file_1 file_2 hash_function
            | "blocks" -> Sync.sync_with_blocks file_1 file_2 block_size hash_function
            | "whitespace" -> Sync.sync_with_whitespace file_1 file_2 block_size hash_function
            | "lines" -> Sync.sync_with_lines file_1 file_2 hash_function
            | _ -> Sync.sync_with_blocks file_1 file_2 block_size hash_function
          in
          Time.time f dest
        end
    end
  | db ->
    begin
      match field_size with
      | "auto" ->
        begin
          let module Sync = Sync_close_bound.Syncing in
          let f =
            match partition with
            | "words" -> Sync.sync_with_words file_1 file_2 hash_function dest
            | "blocks" -> Sync.sync_with_blocks file_1 file_2 block_size hash_function dest
            | "whitespace" -> Sync.sync_with_whitespace file_1 file_2 block_size hash_function dest
            | "lines" -> Sync.sync_with_lines file_1 file_2 hash_function dest
            | _ -> Sync.sync_with_blocks file_1 file_2 block_size hash_function dest
          in
          Time.time f db
        end
      | s ->
        begin
          let f_w = int_of_string s in
          let module F = FiniteField.Make(struct
            let w = f_w
          end)
          in
          let module Sync = Sync_two_files.Syncing(F) in
          let f =
            match partition with
            | "words" -> Sync.sync_with_words file_1 file_2 hash_function dest
            | "blocks" -> Sync.sync_with_blocks file_1 file_2 block_size hash_function dest
            | "whitespace" -> Sync.sync_with_whitespace file_1 file_2 block_size hash_function dest
            | "lines" -> Sync.sync_with_lines file_1 file_2 hash_function dest
            | _ -> Sync.sync_with_blocks file_1 file_2 block_size hash_function dest
          in
          Time.time f db
        end
    end


let local_cmd =
  let file_1 =
    let doc = "File to sync." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE1" ~doc)
  in
  let file_2 =
    let doc = "File present." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"FILE2" ~doc)
  in
  let dest =
    let doc = "Destination file." in
    Arg.(required & pos 2 (some string) None & info [] ~docv:"DEST" ~doc)
  in
  let partition =
    let doc = "Partition function to use." in
    Arg.(required & pos 3 (some string) None & info [] ~docv:"PARTS" ~doc)
  in
  let field_size =
    let doc = "Size w of the finite field GF(2^w)." in
    Arg.(value & opt string "auto" & info ["f"; "field"] ~docv:"FIELD" ~doc)
  in
  let block_size =
    let doc = "Size of the blocks. Only needs to be specified when the partition function requires it." in
    Arg.(value & opt int 4096 & info ["s"; "size"] ~docv:"SIZE" ~doc)
  in
  let db_name =
    let doc = "Database to use." in
    Arg.(value & opt string "none" & info ["d"; "database"] ~docv:"DB_NAME" ~doc)
  in
  let doc = "Syncing locally." in
  let man = [
    `S "DESCRIPTION" ;
    `P "Syncs two files locally. A file to reconstruct at a specified location is provided."]
  in
  Term.(pure local $ file_1 $ file_2 $ dest $ partition $ field_size $ block_size $ db_name),
  Term.info "local" ~doc ~man


(* Default *)
let default_cmd =
  let doc = "tests for syncing" in
  let man = [
    `S "DESCRIPTION" ;
    `P "syncing. With or without network-interaction, with or without database-interaction. Field size can be provided or determined automatically." ]
  in
  Term.(ret (pure (`Ok ()) )) ,
  Term.info "default" ~doc ~man


(* Main *)
let cmds = [load_database_cmd ; server_cmd ; client_cmd ; local_cmd]

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
