  $ dune runtest --display short
      ocamldep .f.eobjs/f.ml.d
        ocamlc .f.eobjs/.byte_objs/f.{cmi,cmo,cmt}
      ocamlopt .f.eobjs/.native_objs/f.{cmx,o}
      ocamlopt f.exe
             f alias runtest
  Foo Bar
  $ dune runtest
  $ dune runtest --force --display short
             f alias runtest
  Foo Bar
