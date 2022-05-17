(lang dune 2.7)

(generate_opam_files true)

(version v0.15)

(package
 (name bonsai)
 (synopsis "Bonsai")
 (description "")
 (depends
  (ocaml
   (>= 4.08.0))
  dune
  core
  virtual_dom
  incr_dom
  incr_map
  async_kernel))
