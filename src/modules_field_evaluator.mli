type t = private
  { all_modules : Module.Name_map.t
  ; virtual_modules : Module.Name_map.t
  }

val eval
  :  modules:Module.Name_map.t
  -> buildable:Dune_file.Buildable.t
  -> virtual_modules:Ordered_set_lang.t option
  -> ?private_modules:(Lib_name.t * Ordered_set_lang.t) list
  -> unit
  -> t
