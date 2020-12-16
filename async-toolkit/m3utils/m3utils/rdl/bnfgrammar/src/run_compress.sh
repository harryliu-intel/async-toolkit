#!/bin/sh -x

../AMD64_LINUX/bnfgrammar -f ../../csrspec/src/csrspec.bnf -r source_text -d ../../csrspec/src -Hy ../../csrspec/src/csrspec.y.0 -Hl ../../csrspec/src/csrspec.l.0 -Ht ../../csrspec/src/csrspec.t.0 -U ../../csrspec/src/csrspec.unify

