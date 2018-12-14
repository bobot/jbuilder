  $ dune utop --display short forutop -- init_forutop.ml
      ocamldep forutop/.utop/utop.ml.d
      ocamldep forutop/.forutop.objs/forutop.ml.d
        ocamlc forutop/.forutop.objs/.byte_objs/forutop.{cmi,cmo,cmt}
        ocamlc forutop/forutop.cma
        ocamlc forutop/.utop/.byte_objs/utop.{cmi,cmo,cmt}
        ocamlc forutop/.utop/utop.exe
  hello in utop
