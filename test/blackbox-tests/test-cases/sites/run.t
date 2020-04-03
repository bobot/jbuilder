Test the `sites` feature

  $ cat > dune-project <<EOF
  > (lang dune 2.5)
  > (package (name a) (sites (share data)))
  > (package (name b) )
  > EOF

  $ touch data_b.txt

  $ cat > dune <<EOF
  > (install (section (site (a data))) (files data_b.txt) (package b))
  > EOF

  $ dune build @install

  $ dune install --prefix _install
  Installing _install/lib/a/META
  Installing _install/lib/a/dune-package
  Installing _install/lib/b/META
  Installing _install/lib/b/dune-package
  Installing _install/share/a/data/data_b.txt

  $ rm data_b.txt

  $ cat > dune-project <<EOF
  > (lang dune 2.5)
  > (package (name c) )
  > EOF

  $ touch data_c.txt

  $ cat > dune <<EOF
  > (install (section (site (a data))) (files data_c.txt) (package c))
  > EOF

  $ OCAMLPATH=_install/lib/:$OCAMLPATH dune build @install

  $ OCAMLPATH=_install/lib/:$OCAMLPATH dune install --prefix _install2
  Installing _install2/lib/c/META
  Installing _install2/lib/c/dune-package
  Installing _install2/share/a/data/data_c.txt
