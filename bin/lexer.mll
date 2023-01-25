{
    open Lexing
    open Parser


    exception SyntaxError of string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p 
        in lexbuf.lex_curr_p <- 
        { 
            pos with pos_bol = lexbuf.lex_curr_pos;
            pos_lnum = pos.pos_lnum + 1
        }
}

    let digit = ['0'-'9']
    let alpha = ['a'-'z' 'A'-'Z']

    let int = ('-' | '+') ? digit+
    let float = ('-' | '+') ? (digit+('.' digit*)? | '.' digit+)
    let char = _?
    let id = (alpha) (alpha | digit '_')*
    let whitespace = [' ' '\t']+ (*whitespace*)
    let newline = '\r' | '\n' | "\r\n" (*newline*)

    rule read_token =
        parse
        | "(" { LPAREN }
        | ")" { RPAREN }
        | "//" { read_single_line_comment lexbuf }
        | "/*" { read_multi_line_comment lexbuf }
        | '"' { read_string (Buffer.create 17) lexbuf }
        | '\'' { read_char lexbuf }
        | '$' { DOLLAR }
        | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
        | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
        | "true" { BOOL (true) }
        | "false" { BOOL (false) }
        | "+" { ADD }
        | "-" { SUB }
        | "*" { MUL }
        | "/" { DIV }
        | "," { COMMA }
        | "!" { EXCLAM }
        | "&&" { AND }
        | "||" { OR }
        | "=" { EQ }
        | ">" { RANGLE }
        | "<" { LANGLE }
        | "return" { RETURN }
        | id { ID (Lexing.lexeme lexbuf) }
        | newline { next_line lexbuf; read_token lexbuf }
        | whitespace { read_token lexbuf }
        | eof { EOF }
        | _ { raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }

    and read_single_line_comment = 
        parse
        | newline { next_line lexbuf; read_token lexbuf }
        | eof { EOF }
        | _ { read_single_line_comment lexbuf }

    and read_multi_line_comment =
        parse
        | "*/" {read_token lexbuf}
        | newline { next_line lexbuf; read_multi_line_comment lexbuf }
        | eof { read_multi_line_comment lexbuf }
        | _ { read_multi_line_comment lexbuf }

    and read_char = 
        parse
        | '\'' { raise (SyntaxError ("Empty char")) }
        | '\\' '/' '\'' { CHAR '/' }
        | '\\' '\\' '\'' { CHAR '\\' }
        | '\\' 'b' '\'' { CHAR '\b' }
        | '\\' 'f' '\'' { CHAR '\012' }
        | '\\' 'n' '\'' { CHAR '\n' }
        | '\\' 'r' '\'' { CHAR '\r' }
        | '\\' 't' '\'' { CHAR '\t' }
        | char '\'' { CHAR (Lexing.lexeme lexbuf).[0]}
        | char+ '\'' { raise (SyntaxError ("Multiple chararacters char"))}
        | _ { raise (SyntaxError ("Illegal character: " ^ Lexing.lexeme lexbuf)) }
        | eof { raise (SyntaxError ("Char is not terminated")) }

    and read_string buf = 
        parse
        | '"'       { STRING (Buffer.contents buf) }
        | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
        | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
        | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
        | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
        | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
        | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
        | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
        | [^ '"' '\\']+
            { 
                Buffer.add_string buf (Lexing.lexeme lexbuf);
                read_string buf lexbuf
            }
        | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
        | eof { raise (SyntaxError ("String is not terminated")) }
