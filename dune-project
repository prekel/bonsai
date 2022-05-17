(lang dune 2.7)

(name bonsai)

(version v0.15)

(license MIT)

(authors "")

(maintainers "")

(generate_opam_files true)

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
