  $ dune runtest --display short
      ocamldep .bar.objs/bar.ml.d
      ocamldep .foo.objs/foo.ml.d
        ocamlc .foo.objs/.byte_objs/foo.{cmi,cmo,cmt}
        ocamlc .bar.objs/.byte_objs/bar.{cmi,cmo,cmt}
        ocamlc bar.cma
      ocamlopt .bar.objs/.native_objs/bar.{cmx,o}
      ocamlopt bar.{a,cmxa}
      ocamlopt bar.cmxs
      ocamlopt .foo.objs/.native_objs/foo.{cmx,o}
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs
        ocamlc foo.cma
     ocamlfind test.exe
          test alias runtest
  42 42
