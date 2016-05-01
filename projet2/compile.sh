#!/bin/bash

ocamlbuild -plugin-tag "package(js_of_ocaml.ocamlbuild)" -use-ocamlfind observer_lib.cma
