(** Maps from file and md5 to filenames. *)
module M = struct
  module O = struct
    type t = int * Digest.t
    let compare = compare
  end
  module M = Map.Make(O)

  type t = string list M.t

  let empty = M.empty

  let add m fname =
    let s =
      let ic = open_in_bin fname in
      let len = in_channel_length ic in
      let s = really_input_string ic len in
      close_in ic;
      s
    in
    M.add_to_list (String.length s, Digest.string s) fname m

  let iter m f =
    M.iter f m
end

let () =
  let files = ref [] in
  Arg.parse
    (Arg.align
       [
       ]
    )
    (fun s -> files := s :: !files)
    "samefind options files";
  let rec find fname =
    if Sys.is_directory fname then
      let dir = fname in
      dir |> Sys.readdir |> Array.to_list |> List.map (fun f -> if dir = "." then f else Filename.concat dir f) |> List.map find |> List.flatten
    else
      [fname]
  in
  let files = List.map find !files |> List.flatten |> List.sort compare in
  Printf.printf "Considering %d files.\n\n%!" (List.length files);
  (* List.iter (fun f -> Printf.printf "- considering %s\n%!" f) files; *)
  let m = List.fold_left M.add M.empty files in
  M.iter m
    (fun (_n,md5) l ->
       if List.length l > 1 then
         let l = List.sort compare l in
         print_endline (Digest.to_hex md5 ^ ":");
         List.iter (fun f -> print_endline ("- " ^ f)) l
    )
