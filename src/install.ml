open Import

module Section = struct
  type t =
    | Lib
    | Libexec
    | Bin
    | Sbin
    | Toplevel
    | Share
    | Share_root
    | Etc
    | Doc
    | Stublibs
    | Man
    | Misc

  let compare : t -> t -> int = compare

  let to_string = function
    | Lib        -> "lib"
    | Libexec    -> "libexec"
    | Bin        -> "bin"
    | Sbin       -> "sbin"
    | Toplevel   -> "toplevel"
    | Share      -> "share"
    | Share_root -> "share_root"
    | Etc        -> "etc"
    | Doc        -> "doc"
    | Stublibs   -> "stublibs"
    | Man        -> "man"
    | Misc       -> "misc"

  let t =
    let open Sexp.Of_sexp in
    enum
      [ "lib"        , Lib
      ; "libexec"    , Libexec
      ; "bin"        , Bin
      ; "sbin"       , Sbin
      ; "toplevel"   , Toplevel
      ; "share"      , Share
      ; "share_root" , Share_root
      ; "etc"        , Etc
      ; "doc"        , Doc
      ; "stublibs"   , Stublibs
      ; "man"        , Man
      ; "misc"       , Misc
      ]

  (** Installed files must be set executable *)
  let exec = function
    | Bin
    | Sbin
    | Libexec
    | Stublibs -> true
    | Lib
    | Toplevel
    | Share
    | Share_root
    | Etc
    | Doc
    | Man
    | Misc -> false

end

module Entry = struct
  type t =
    { src     : Path.t
    ; dst     : string option
    ; section : Section.t
    }

  let make section ?dst src =
    let dst =
      if Sys.win32 then
        let src_base = Path.basename src in
        let dst' =
          match dst with
          | None -> src_base
          | Some s -> s
        in
        match Filename.extension src_base with
        | ".exe" | ".bc" ->
          if Filename.extension dst' <> ".exe" then
            Some (dst' ^ ".exe")
          else
            dst
        | _ -> dst
      else
        dst
    in
    { src
    ; dst
    ; section
    }

  let set_src t src = { t with src }

  type layout = {
    (** ocaml lib *)
    lib         : Path.t;
    libexec     : Path.t;
    bin         : Path.t;
    sbin        : Path.t;
    toplevel    : Path.t;
    share       : Path.t;
    share_root  : Path.t;
    etc         : Path.t;
    doc         : Path.t;
    stublibs    : Path.t;
    man         : Path.t;
  }

  let local_layout ~prefix ~libdir = {
    lib         = Path.relative prefix "lib";
    libexec     = Path.relative prefix "lib";
    bin         = Path.relative prefix "bin";
    sbin        = Path.relative prefix "sbin";
    toplevel    = Path.relative prefix "lib/toplevel";
    share       = Path.relative prefix "share";
    share_root  = Path.relative prefix "share_root";
    etc         = Path.relative prefix "etc";
    doc         = Path.relative prefix "doc";
    stublibs    = Path.relative prefix "lib/stublibs";
    man         = Path.relative prefix "man";
  }

  let layout ~libdir ~prefix = {
    lib         = libdir;
    libexec     = libdir;
    bin         = Path.relative prefix "bin";
    sbin        = Path.relative prefix "sbin";
    toplevel    = Path.relative libdir "toplevel";
    share       = Path.relative prefix "share";
    share_root  = Path.relative prefix "share_root";
    etc         = Path.relative prefix "etc";
    doc         = Path.relative prefix "doc";
    stublibs    = Path.relative libdir "stublibs";
    man         = Path.relative prefix "man";
  }

  let relative_installed_path ~layout t ~package =
    let main_dir =
      match t.section with
      | Bin        -> layout.bin
      | Sbin       -> layout.sbin
      | Toplevel   -> layout.toplevel
      | Share_root -> layout.share_root
      | Stublibs   -> layout.stublibs
      | Man        -> layout.man
      | Lib        -> Path.relative layout.lib     package
      | Libexec    -> Path.relative layout.libexec package
      | Share      -> Path.relative layout.share   package
      | Etc        -> Path.relative layout.etc     package
      | Doc        -> Path.relative layout.doc     package
      | Misc       -> invalid_arg "Install.Entry.relative_installed_path"
    in
    let dst =
      match t.dst with
      | Some x -> x
      | None ->
        let dst = Path.basename t.src in
        match t.section with
        | Man -> begin
            match String.rsplit2 dst ~on:'.' with
            | None -> dst
            | Some (_, sec) -> sprintf "man%s/%s" sec dst
          end
        | _ -> dst
    in
    Path.relative main_dir dst

  let path_opam prefix pkg = function
    | Bin      -> Path.relative prefix "bin"
    | Sbin     -> Path.relative prefix "sbin"
    | Etc      -> Path.relative prefix "etc"
    | Stublibs -> Path.relative prefix "lib/stublibs"
    | Toplevel -> Path.relative prefix "lib/toplevel"
    | Libexec
    | Lib      -> Path.relative (Path.relative prefix "lib") pkg
    | Share    -> Path.relative (Path.relative prefix "share") pkg
    | Share_root -> Path.relative prefix "share"
    | Doc      -> Path.relative (Path.relative prefix "doc") pkg
    | Man      -> Path.relative prefix "man"
    | Misc     -> assert false (** TODO : ask  the user *)


end

module SMap = Map.Make(Section)

let files entries =
  List.fold_left entries ~init:Path.Set.empty ~f:(fun acc (entry : Entry.t) ->
    Path.Set.add entry.src acc)

let group entries =
  List.map entries ~f:(fun (entry : Entry.t) -> (entry.section, entry))
  |> SMap.of_alist_multi
  |> SMap.bindings

let gen_install_file entries =
  let buf = Buffer.create 4096 in
  let pr fmt = Printf.bprintf buf (fmt ^^ "\n") in
  List.iter (group entries) ~f:(fun (section, entries) ->
    pr "%s: [" (Section.to_string section);
      List.iter entries ~f:(fun (e : Entry.t) ->
        let src = Path.to_string e.src in
        match e.dst with
        | None     -> pr "  %S"      src
        | Some dst -> pr "  %S {%S}" src dst);
    pr "]");
  Buffer.contents buf

let install entry =
  Path.mkdir_p
