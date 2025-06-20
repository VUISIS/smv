# Smvlib - An Ocaml library for parsing/writing SMV files

This library contains an AST for the SMV model checking language
used by NuSMV and nuXmv. It can parse SMV files and also generate
them.

To parse a file, use `Smv_io.parse_smv`, like:

```ocaml
Smv_io.parse_smv "myfile.smv"
```

The `parse_smv` function returns a value of type `Smv.Program`.

To write a value of type `Smv.Program` use `Smv_io.write_smv`:

```ocaml
Smv.io.write_smv "outfile.smv" some_program
```

This would essentially make a copy of an SMV file, with
possible differences in whitespace and parentheses in
expressions:

```ocaml
Smv_io.write_smv "outfile.smv" (Smv_io.parse_smv "infile.smv")
```
