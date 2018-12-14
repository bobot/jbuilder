  $ dune exec ./main.exe --display short
          echo main.ml
      ocamldep .main.eobjs/main.ml.d
        ocamlc .main.eobjs/.byte_objs/main.{cmi,cmo,cmt}
      ocamlopt .main.eobjs/.native_objs/main.{cmx,o}
      ocamlopt main.exe
  Hello World
