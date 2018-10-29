(*i camlp4deps: "grammar/grammar.cma" i*)

let wit_reference = Constrarg.wit_ref

DECLARE PLUGIN "rawprint"

VERNAC COMMAND EXTEND Showp CLASSIFIED AS QUERY

| [ "Print" "Raw" "ProofTerm" ] ->
  [ Rawprint.print_proof_term () ]

| [ "Print" "Raw" "Reference" reference(r) ] ->
  [ Rawprint.print_reference r ]
END
