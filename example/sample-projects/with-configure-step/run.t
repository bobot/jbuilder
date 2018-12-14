  $ dune build @install @runtest --display short
         ocaml config.full
      ocamldep src/.plop.eobjs/config.ml.d
      ocamldep src/.plop.eobjs/plop.ml.d
        ocamlc src/.plop.eobjs/.byte_objs/config.{cmi,cmo,cmt}
        ocamlc src/.plop.eobjs/.byte_objs/plop.{cmi,cmo,cmt}
      ocamlopt src/.plop.eobjs/.native_objs/plop.{cmx,o}
      ocamlopt src/.plop.eobjs/.native_objs/config.{cmx,o}
      ocamlopt src/plop.exe
