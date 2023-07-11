(lang dune 2.8)

(using dune_site 0.1)

(name piaf)

(generate_opam_files true)

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
   (>= "5.0"))
  logs
  (eio-ssl :dev)
  magic-mime
  (ssl :dev)
  uri
  ipaddr
  (httpaf-eio :dev)
  (gluten-eio :dev)
  (h2-eio :dev)
  (websocketaf :dev)
  (multipart_form :dev)
  (eio_main :dev)
  (dune-site :with-test)
  (alcotest :with-test)))
