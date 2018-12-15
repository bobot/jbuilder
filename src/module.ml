open! Stdune
open Import

module Syntax = struct
  type t = OCaml | Reason

  let to_string = function
    | OCaml -> "ocaml"
    | Reason -> "reason"

  let pp fmt t = Format.pp_print_string fmt (to_string t)

  let to_sexp t = Sexp.Encoder.string (to_string t)
end

module Name = struct
  module T = struct
    type t = string
    let compare = compare
  end

  include T

  let decode = Dune_lang.Decoder.string
  let encode = Dune_lang.Encoder.string

  let to_sexp = Sexp.Encoder.string

  let add_suffix = (^)

  let of_string = String.capitalize
  let to_string x = x

  let uncapitalize = String.uncapitalize

  let pp = Format.pp_print_string
  let pp_quote fmt x = Format.fprintf fmt "%S" x

  module Set = struct
    include String.Set
  end
  module Map = String.Map
  module Top_closure = Top_closure.String
  module Infix = Comparable.Operators(T)

  let of_local_lib_name s =
    of_string (Lib_name.Local.to_string s)

  let to_local_lib_name s =
    Lib_name.Local.of_string_exn s

  let basename n ~(ml_kind : Ml_kind.t) ~(syntax : Syntax.t) =
    let ext =
      match syntax, ml_kind with
      | Reason, Intf -> ".rei"
      | Reason, Impl -> ".re"
      | OCaml, Intf -> ".mli"
      | OCaml, Impl -> ".ml"
    in
    String.lowercase n ^ ext
end

module File = struct
  type t =
    { path   : Path.t
    ; syntax : Syntax.t
    }

  let make syntax path = { syntax; path }

  let to_sexp { path; syntax } =
    let open Sexp.Encoder in
    record
      [ "path", Path.to_sexp path
      ; "syntax", Syntax.to_sexp syntax
      ]

  let pp fmt { path; syntax } =
    Fmt.record fmt
      [ "path", Fmt.const Path.pp path
      ; "syntax", Fmt.const Syntax.pp syntax
      ]
end

type t =
  { name       : Name.t
  ; impl       : File.t option
  ; intf       : File.t option
  ; obj_name   : string
  ; pp         : (unit, string list) Build.t option
  ; interfaces : Lib_name.Set.t
  }

let name t = t.name
let pp_flags t = t.pp
let intf t = t.intf
let impl t = t.impl

let make ?impl ?intf ?obj_name ?(interfaces=Lib_name.Set.empty) name =
  let file : File.t =
    match impl, intf with
    | None, None ->
      Exn.code_error "Module.make called with no files"
        [ "name", Sexp.Encoder.string name
        ; "impl", Sexp.Encoder.(option unknown) impl
        ; "intf", Sexp.Encoder.(option unknown) intf
        ]
    | Some file, _
    | _, Some file -> file
  in
  let obj_name =
    match obj_name with
    | Some s -> s
    | None ->
      let fn = Path.basename file.path in
      match String.index fn '.' with
      | None   -> fn
      | Some i -> String.take fn i
  in
  { name
  ; impl
  ; intf
  ; obj_name
  ; pp = None
  ; interfaces
  }

let real_unit_name t = Name.of_string (Filename.basename t.obj_name)

let has_impl t = Option.is_some t.impl
let has_intf t = Option.is_some t.intf

let impl_only t = has_impl t && not (has_intf t)
let intf_only t = has_intf t && not (has_impl t)

let file t (kind : Ml_kind.t) =
  let file =
    match kind with
    | Impl -> t.impl
    | Intf -> t.intf
  in
  Option.map file ~f:(fun f -> f.path)

let obj_file t ~obj_dir ~mode ~ext =
  let base =
    match mode with
    | Mode.Byte -> Utils.library_byte_dir ~obj_dir
    | Native -> Utils.library_native_dir ~obj_dir
  in
  Path.relative base (t.obj_name ^ ext)

let obj_name t = t.obj_name

let cm_source t kind = file t (Cm_kind.source kind)

let cm_file_unsafe t ?ext ~obj_dir kind =
  let mode = match kind with Cm_kind.Cmx -> Mode.Native | Cmo | Cmi -> Byte in
  let ext = Option.value ext ~default:(Cm_kind.ext kind) in
  obj_file t ~obj_dir ~mode ~ext

let cm_file t ?ext ~obj_dir (kind : Cm_kind.t) =
  match kind with
  | (Cmx | Cmo) when not (has_impl t) -> None
  | _ -> Some (cm_file_unsafe t ?ext ~obj_dir kind)

let cm_public_file_unsafe t ?ext ~obj_dir ~intf kind =
  let ext = Option.value ext ~default:(Cm_kind.ext kind) in
  let base = match kind with
    | Cm_kind.Cmx -> Utils.library_native_dir ~obj_dir
    | Cmo -> Utils.library_byte_dir ~obj_dir
    | Cmi -> Utils.library_public_cmi_dir ~obj_dir ~intf in
  Path.relative base (t.obj_name ^ ext)

let cm_public_file t ?ext ~obj_dir ~intf (kind : Cm_kind.t) =
  match kind with
  | (Cmx | Cmo) when not (has_impl t) -> None
  |  Cmi when not (Lib_name.Set.mem t.interfaces intf) -> None
  | _ -> Some (cm_public_file_unsafe t ?ext ~obj_dir ~intf kind)

let cmt_file t ~obj_dir (kind : Ml_kind.t) =
  match kind with
  | Impl -> Option.map t.impl ~f:(fun _ -> obj_file t ~mode:Byte ~obj_dir ~ext:".cmt" )
  | Intf -> Option.map t.intf ~f:(fun _ -> obj_file t ~mode:Byte ~obj_dir ~ext:".cmti")

let odoc_file t ~doc_dir = Path.relative doc_dir (t.obj_name ^ ".odoc")

let cmti_file t ~obj_dir =
  match t.intf with
  | None   -> obj_file t ~obj_dir ~mode:Byte ~ext:".cmt"
  | Some _ -> obj_file t ~obj_dir ~mode:Byte ~ext:".cmti"

let iter t ~f =
  Option.iter t.impl ~f:(f Ml_kind.Impl);
  Option.iter t.intf ~f:(f Ml_kind.Intf)

let with_wrapper t ~main_module_name =
  { t with obj_name
           = sprintf "%s__%s"
               (String.uncapitalize main_module_name) t.name
  }

let map_files t ~f =
  { t with
    impl = Option.map t.impl ~f:(f Ml_kind.Impl)
  ; intf = Option.map t.intf ~f:(f Ml_kind.Intf)
  }

let src_dir t =
  match t.intf, t.impl with
  | None, None -> None
  | Some x, Some _
  | Some x, None
  | None, Some x -> Some (Path.parent_exn x.path)

let set_pp t pp = { t with pp }

let to_sexp { name; impl; intf; obj_name ; pp ; interfaces } =
  let open Sexp.Encoder in
  record
    [ "name", Name.to_sexp name
    ; "obj_name", string obj_name
    ; "impl", (option File.to_sexp) impl
    ; "intf", (option File.to_sexp) intf
    ; "pp", (option string) (Option.map ~f:(fun _ -> "has pp") pp)
    ; "interfaces", (list Lib_name.to_sexp) (Lib_name.Set.to_list interfaces)
    ]

let pp fmt { name; impl; intf; obj_name ; pp = _ ; interfaces } =
  Fmt.record fmt
    [ "name", Fmt.const Name.pp name
    ; "impl", Fmt.const (Fmt.optional File.pp) impl
    ; "intf", Fmt.const (Fmt.optional File.pp) intf
    ; "obj_name", Fmt.const Format.pp_print_string obj_name
    ; "interfaces", Fmt.const (Fmt.list Lib_name.pp) (Lib_name.Set.to_list interfaces)
    ]

let wrapped_compat t =
  { t with
    intf = None
  ; impl =
      Some (
        { syntax = OCaml
        ; path =
            (* Option.value_exn cannot fail because we disallow wrapped
               compatibility mode for virtual libraries. That means none of the
               modules are implementing a virtual module, and therefore all have
               a source dir *)
            Path.L.relative (Option.value_exn (src_dir t))
              [ ".wrapped_compat"
              ; Name.to_string t.name ^ ".ml-gen"
              ]
        }
      )
  }

module Name_map = struct
  type nonrec t = t Name.Map.t

  let impl_only =
    Name.Map.fold ~init:[] ~f:(fun m acc ->
      if has_impl m then
        m :: acc
      else
        acc)

  let of_list_exn modules =
    List.map modules ~f:(fun m -> (name m, m))
    |> Name.Map.of_list_exn

  let add t module_ =
    Name.Map.add t (name module_) module_
end

let set_interfaces t interfaces =
  { t with interfaces }
let interfaces t = t.interfaces
let mem_interfaces t i = Lib_name.Set.mem t.interfaces i

let visibility t = t.visibility

let remove_files t =
  { t with
    intf = None
  ; impl = None
  }

let sources t =
  List.filter_map [t.intf; t.impl]
    ~f:(Option.map ~f:(fun (x : File.t) -> x.path))

module Obj_map = struct
  include Map.Make(struct
      type nonrec t = t
      let compare m1 m2 = String.compare m1.obj_name m2.obj_name
    end)

  let top_closure t =
    Top_closure.String.top_closure
      ~key:(fun m -> m.obj_name)
      ~deps:(fun m ->
        match find t m with
        | Some m -> m
        | None ->
          Exn.code_error "top_closure: unable to find key"
            [ "m", to_sexp m
            ; "t", (Sexp.Encoder.list to_sexp) (keys t)
            ])
end

let encode
      ({ name
       ; impl = _
       ; intf = _
       ; obj_name
       ; pp = _
       ; visibility
       } as t) =
  let open Dune_lang.Encoder in
  record_fields Dune
    [ field "name" Name.encode name
    ; field "obj_name" string obj_name
    ; field "visibility" Visibility.encode visibility
    ; field_o_b "impl" (has_impl t)
    ; field_o_b "intf" (has_intf t)
    ]

let decode ~dir =
  let open Dune_lang.Decoder in
  fields (
    let%map name = field "name" Name.decode
    and obj_name = field "obj_name" string
    and visibility = field "visibility" Visibility.decode
    and impl = field_b "impl"
    and intf = field_b "intf"
    in
    let file exists ml_kind =
      if exists then
        let basename = Name.basename name ~ml_kind ~syntax:OCaml in
        Some (File.make Syntax.OCaml (Path.relative dir basename))
      else
        None
    in
    let intf = file intf Intf in
    let impl = file impl Impl in
    make ~obj_name ~visibility ?impl ?intf name
  )
