(lang dune 2.8)

(using dune_site 0.1)

(name piaf)

(generate_opam_files true)

(use_standard_c_and_cxx_flags true)

(source
 (github anmonteiro/piaf))

(authors "Antonio Nuno Monteiro <anmonteiro@gmail.com>")

(maintainers "Antonio Nuno Monteiro <anmonteiro@gmail.com>")

(homepage "https://github.com/anmonteiro/piaf")

(bug_reports "https://github.com/anmonteiro/piaf/issues")

(license "BSD-3-clause")

(package
 (name piaf)
 (synopsis "An HTTP library with HTTP/2 support written entirely in OCaml")
 (description
  "Piaf is an HTTP library and webserver written entirely in OCaml.")
 (depends
  (ocaml
   (>= "5.1"))
  logs
  (eio-ssl
   (>= 0.3.0))
  magic-mime
  ssl
  uri
  ipaddr
  (httpun-eio
   (>= 0.2.0))
  gluten-eio
  (h2-eio
   (>= 0.13.0))
  (httpun-ws
   (>= 0.2.0))
  (pecu
   (>= 0.4))
  prettym
  (unstrctrd
   (>= 0.2))
  (eio_main
   (>= "1.0"))
  (dune-site :with-test)
  (alcotest :with-test)))
