open Ocamlbuild_plugin

let _ = Options.ocamlc := S [ A "ocamlc"; A "-g" ]
let _ = Options.ocamlopt := S [ A "ocamlopt"; A "-inline"; A "20"; A "-unsafe"; A "-noassert" ]
