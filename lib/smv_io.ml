let parse_smv filename =
  In_channel.with_open_text filename
    (fun file ->
      let lexbuf = Lexing.from_channel file in
      try
        Smv_parser.program Smv_lexer.token lexbuf
      with _ ->
        (Printf.printf "Error on line %d:%d\n" lexbuf.lex_curr_p.pos_lnum
           (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol);
         failwith "parse error"))

let write_smv filename prog =
  Out_channel.with_open_text filename
    (fun file ->
      Printf.fprintf file "%s\n" (Smv_buffer.string_of_program prog))
  
