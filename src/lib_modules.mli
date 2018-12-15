open Stdune

type t

module Alias_module : sig
  type t = private
    { main_module_name : Module.Name.t
    ; alias_module : Module.t
    }
end

val alias : t -> Alias_module.t option

val alias_module : t -> Module.t option

val modules : t -> Module.Name_map.t

val wrapped_compat : t -> Module.Name_map.t

val main_module_name : t -> Module.Name.t option

val virtual_modules : t -> Module.Name_map.t

val installable_modules : t -> Module.t list

val lib_interface_module : t -> Module.t option

val entry_modules : t -> Module.t list

val make
  :  Dune_file.Library.t
  -> dir:Path.t
  -> Module.Name_map.t
  -> virtual_modules:Module.Name_map.t
  -> main_module_name:Module.Name.t option
  -> t

val set_modules : t -> Module.Name_map.t -> t

val for_compilation : t -> Module.Name_map.t

val have_artifacts : t -> Module.Name_map.t

module Virtual : sig
  val encode : t -> Dune_lang.t list
  val decode
    :  main_module_name:Module.Name.t
    -> dir:Path.t
    -> t Dune_lang.Decoder.t
end

val encode : t -> Dune_lang.t list

val decode : implements:bool -> dir:Path.t -> t Dune_lang.Decoder.t
