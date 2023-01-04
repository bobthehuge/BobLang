open Bobcore;;
open Memory;;
open Lexer;;
open Printf;;
open Lexing;;
open Types;;

(*let too_much_args fct = failwith (Printf.sprintf "%s: too much arguments" fct);;
let not_enou_args fct = failwith (Printf.sprintf "%s: not enough arguments" fct);;
let syntax_error() = failwith "Syntax Error";;*)

let unkn_file() = failwith "Unknown BobLang file (should be *.bl)";;


let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:line %d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read_token lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    Null
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

(*let rec parse_and_print lexbuf =
    let tok = parse_with_error lexbuf
    in match tok with
    | Int _ | Float _ | Char _ | String _
    | Set _ | Add _ | Print _ | Println _ ->
            fprintf stdout "%s\n" (debug tok);
            parse_and_print lexbuf

    | Null -> parse_and_print lexbuf
    | Exit -> ()*)

let rec parse_and_exec lexbuf =
    let tok = parse_with_error lexbuf
    in if tok = Eof then ()
    else let _ = eval tok
    in parse_and_exec lexbuf

let loop filename () =
  let inx = open_in filename 
  in let lexbuf = Lexing.from_channel inx 
  in (lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      parse_and_exec lexbuf;
      close_in inx)

let () =
    let file = Sys.argv.(1)
    in if String.length file < 4 then unkn_file()
    else if String.sub file ((String.length file) - 3) (3) <> ".bl" then
        unkn_file()
    else loop Sys.argv.(1) ()
    
