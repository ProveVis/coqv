# 1 "xml_lexer.mll"
 (*
 * Xml Light, an small Xml parser/printer with DTD support.
 * Copyright (C) 2003 Nicolas Cannasse (ncannasse@motion-twin.com)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Lexing

type error =
        | EUnterminatedComment
        | EUnterminatedString
        | EIdentExpected
        | ECloseExpected
        | ENodeExpected
        | EAttributeNameExpected
        | EAttributeValueExpected
        | EUnterminatedEntity

exception Error of error

type pos = int * int * int * int

type token =
        | Tag of string * (string * string) list * bool
        | PCData of string
        | Endtag of string
        | Eof

let last_pos = ref 0
and current_line = ref 0
and current_line_start = ref 0

let tmp = Buffer.create 200

let idents = Hashtbl.create 0

let _ = begin
        Hashtbl.add idents "nbsp;" " ";
        Hashtbl.add idents "gt;" ">";
        Hashtbl.add idents "lt;" "<";
        Hashtbl.add idents "amp;" "&";
        Hashtbl.add idents "apos;" "'";
        Hashtbl.add idents "quot;" "\"";
end

let init lexbuf =
        current_line := 1;
        current_line_start := lexeme_start lexbuf;
        last_pos := !current_line_start

let close lexbuf =
        Buffer.reset tmp

let pos lexbuf =
        !current_line , !current_line_start ,
        !last_pos ,
        lexeme_start lexbuf

let restore (cl,cls,lp,_) =
        current_line := cl;
        current_line_start := cls;
        last_pos := lp

let newline lexbuf =
        incr current_line;
        last_pos := lexeme_end lexbuf;
        current_line_start := !last_pos

let error lexbuf e =
        last_pos := lexeme_start lexbuf;
        raise (Error e)


# 89 "xml_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\246\255\247\255\001\000\000\000\008\000\255\255\002\000\
    \000\000\010\000\253\255\000\000\001\000\254\255\250\255\011\000\
    \016\000\255\255\003\000\013\000\252\255\253\255\002\000\255\255\
    \005\000\002\000\254\255\017\000\252\255\253\255\003\000\255\255\
    \009\000\254\255\021\000\001\000\039\000\255\255\015\000\253\255\
    \037\000\254\255\101\000\255\255\231\000\040\001\254\255\118\001\
    \004\000\254\255\255\255\006\000\005\000\255\255\254\255\183\001\
    \254\255\005\002\024\000\253\255\061\000\215\000\216\000\217\000\
    \254\255\255\255\038\000\251\255\252\255\253\255\039\000\255\255\
    \254\255\036\000\251\255\252\255\253\255\005\000\255\255\254\255\
    ";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\007\000\006\000\004\000\255\255\000\000\
    \003\000\004\000\255\255\255\255\255\255\255\255\255\255\002\000\
    \001\000\255\255\000\000\255\255\255\255\255\255\003\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\003\000\255\255\
    \000\000\255\255\004\000\003\000\001\000\255\255\000\000\255\255\
    \255\255\255\255\001\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\002\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\002\000\002\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\004\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\004\000\255\255\255\255\
    ";
  Lexing.lex_default = 
   "\003\000\000\000\000\000\003\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\000\000\255\255\255\255\000\000\000\000\255\255\
    \255\255\000\000\255\255\020\000\000\000\000\000\255\255\000\000\
    \255\255\255\255\000\000\028\000\000\000\000\000\255\255\000\000\
    \255\255\000\000\036\000\255\255\036\000\000\000\255\255\000\000\
    \041\000\000\000\255\255\000\000\255\255\046\000\000\000\255\255\
    \049\000\000\000\000\000\255\255\255\255\000\000\000\000\056\000\
    \000\000\255\255\059\000\000\000\255\255\255\255\255\255\255\255\
    \000\000\000\000\067\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\074\000\000\000\000\000\000\000\255\255\000\000\000\000\
    ";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\008\000\007\000\255\255\000\000\006\000\255\255\006\000\
    \017\000\009\000\023\000\009\000\016\000\018\000\031\000\024\000\
    \017\000\016\000\023\000\032\000\037\000\000\000\031\000\038\000\
    \008\000\061\000\037\000\014\000\039\000\000\000\004\000\255\255\
    \009\000\011\000\009\000\016\000\079\000\012\000\013\000\025\000\
    \016\000\255\255\000\000\000\000\255\255\052\000\000\000\008\000\
    \061\000\008\000\022\000\035\000\005\000\255\255\001\000\255\255\
    \026\000\033\000\050\000\054\000\053\000\000\000\062\000\010\000\
    \071\000\072\000\076\000\078\000\069\000\255\255\000\000\000\000\
    \030\000\255\255\000\000\255\255\000\000\060\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\062\000\000\000\065\000\
    \000\000\079\000\000\000\255\255\064\000\255\255\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \077\000\000\000\070\000\072\000\000\000\000\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \043\000\000\000\000\000\000\000\000\000\000\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \063\000\062\000\063\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\063\000\
    \062\000\063\000\065\000\000\000\000\000\000\000\000\000\064\000\
    \002\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\021\000\000\000\000\000\
    \000\000\029\000\000\000\000\000\062\000\255\255\062\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\043\000\000\000\075\000\000\000\068\000\255\255\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\047\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\000\000\000\000\000\000\000\000\047\000\
    \000\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\000\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\000\000\000\000\000\000\000\000\000\000\000\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\000\000\000\000\000\000\000\000\047\000\000\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\057\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\000\000\000\000\000\000\000\000\057\000\000\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\000\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \000\000\000\000\000\000\000\000\057\000\000\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\008\000\000\000\003\000\255\255\000\000\003\000\007\000\
    \018\000\005\000\024\000\009\000\015\000\015\000\032\000\019\000\
    \015\000\016\000\019\000\027\000\038\000\255\255\027\000\034\000\
    \008\000\058\000\034\000\004\000\035\000\255\255\000\000\003\000\
    \005\000\005\000\009\000\015\000\077\000\011\000\012\000\022\000\
    \016\000\036\000\255\255\255\255\036\000\051\000\255\255\005\000\
    \058\000\009\000\019\000\034\000\000\000\003\000\000\000\003\000\
    \025\000\030\000\048\000\052\000\051\000\255\255\060\000\005\000\
    \066\000\070\000\073\000\073\000\066\000\036\000\255\255\255\255\
    \027\000\034\000\255\255\034\000\255\255\058\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\060\000\255\255\060\000\
    \255\255\077\000\255\255\036\000\060\000\036\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \073\000\255\255\066\000\070\000\255\255\255\255\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
    \042\000\255\255\255\255\255\255\255\255\255\255\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \061\000\062\000\063\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\061\000\
    \062\000\063\000\062\000\255\255\255\255\255\255\255\255\062\000\
    \000\000\003\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\019\000\255\255\255\255\
    \255\255\027\000\255\255\255\255\061\000\034\000\063\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\044\000\255\255\073\000\255\255\066\000\036\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\045\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\255\255\255\255\255\255\255\255\045\000\
    \255\255\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\047\000\047\000\255\255\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\255\255\255\255\255\255\255\255\255\255\255\255\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\255\255\255\255\255\255\255\255\047\000\255\255\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\055\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\255\255\255\255\255\255\255\255\055\000\255\255\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\057\000\057\000\255\255\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \255\255\255\255\255\255\255\255\057\000\255\255\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 98 "xml_lexer.mll"
                (
                        newline lexbuf;
                        PCData "\n"
                )
# 347 "xml_lexer.ml"

  | 1 ->
# 103 "xml_lexer.mll"
                (
                        last_pos := lexeme_start lexbuf;
                        comment lexbuf;
                        token lexbuf
                )
# 356 "xml_lexer.ml"

  | 2 ->
# 109 "xml_lexer.mll"
                (
                        last_pos := lexeme_start lexbuf;
                        header lexbuf;
                        token lexbuf;
                )
# 365 "xml_lexer.ml"

  | 3 ->
# 115 "xml_lexer.mll"
                (
                        last_pos := lexeme_start lexbuf;
                        let tag = ident_name lexbuf in
                        ignore_spaces lexbuf;
                        close_tag lexbuf;
                        Endtag tag
                )
# 376 "xml_lexer.ml"

  | 4 ->
# 123 "xml_lexer.mll"
                (
                        last_pos := lexeme_start lexbuf;
                        let tag = ident_name lexbuf in
                        ignore_spaces lexbuf;
                        let attribs, closed = attributes lexbuf in
                        Tag(tag, attribs, closed)
                )
# 387 "xml_lexer.ml"

  | 5 ->
# 131 "xml_lexer.mll"
                (
                        last_pos := lexeme_start lexbuf;
                        Buffer.reset tmp;
                        Buffer.add_string tmp (lexeme lexbuf);
                        PCData (pcdata lexbuf)
                )
# 397 "xml_lexer.ml"

  | 6 ->
# 138 "xml_lexer.mll"
                (
                        last_pos := lexeme_start lexbuf;
                        Buffer.reset tmp;
                        Buffer.add_string tmp (entity lexbuf);
                        PCData (pcdata lexbuf)
                )
# 407 "xml_lexer.ml"

  | 7 ->
# 145 "xml_lexer.mll"
                (
                        last_pos := lexeme_start lexbuf;
                        Buffer.reset tmp;
                        Buffer.add_string tmp (lexeme lexbuf);
                        PCData (pcdata lexbuf)
                )
# 417 "xml_lexer.ml"

  | 8 ->
# 151 "xml_lexer.mll"
              ( Eof )
# 422 "xml_lexer.ml"

  | 9 ->
# 153 "xml_lexer.mll"
                ( error lexbuf ENodeExpected )
# 427 "xml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and ignore_spaces lexbuf =
    __ocaml_lex_ignore_spaces_rec lexbuf 15
and __ocaml_lex_ignore_spaces_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 157 "xml_lexer.mll"
                (
                        newline lexbuf;
                        ignore_spaces lexbuf
                )
# 442 "xml_lexer.ml"

  | 1 ->
# 162 "xml_lexer.mll"
                ( ignore_spaces lexbuf )
# 447 "xml_lexer.ml"

  | 2 ->
# 164 "xml_lexer.mll"
                ( () )
# 452 "xml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_ignore_spaces_rec lexbuf __ocaml_lex_state

and comment lexbuf =
    __ocaml_lex_comment_rec lexbuf 19
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 168 "xml_lexer.mll"
                (
                        newline lexbuf;
                        comment lexbuf
                )
# 467 "xml_lexer.ml"

  | 1 ->
# 173 "xml_lexer.mll"
                ( () )
# 472 "xml_lexer.ml"

  | 2 ->
# 175 "xml_lexer.mll"
                ( raise (Error EUnterminatedComment) )
# 477 "xml_lexer.ml"

  | 3 ->
# 177 "xml_lexer.mll"
                ( comment lexbuf )
# 482 "xml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

and header lexbuf =
    __ocaml_lex_header_rec lexbuf 27
and __ocaml_lex_header_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 181 "xml_lexer.mll"
                (
                        newline lexbuf;
                        header lexbuf
                )
# 497 "xml_lexer.ml"

  | 1 ->
# 186 "xml_lexer.mll"
                ( () )
# 502 "xml_lexer.ml"

  | 2 ->
# 188 "xml_lexer.mll"
                ( error lexbuf ECloseExpected )
# 507 "xml_lexer.ml"

  | 3 ->
# 190 "xml_lexer.mll"
                ( header lexbuf )
# 512 "xml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_header_rec lexbuf __ocaml_lex_state

and pcdata lexbuf =
    __ocaml_lex_pcdata_rec lexbuf 34
and __ocaml_lex_pcdata_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 194 "xml_lexer.mll"
                (
                        Buffer.add_char tmp '\n';
                        newline lexbuf;
                        pcdata lexbuf
                )
# 528 "xml_lexer.ml"

  | 1 ->
# 200 "xml_lexer.mll"
                (
                        Buffer.add_string tmp (lexeme lexbuf);
                        pcdata lexbuf
                )
# 536 "xml_lexer.ml"

  | 2 ->
# 205 "xml_lexer.mll"
                (
                        Buffer.add_string tmp (lexeme lexbuf);
                        pcdata lexbuf;
                )
# 544 "xml_lexer.ml"

  | 3 ->
# 210 "xml_lexer.mll"
                (
                        Buffer.add_string tmp (entity lexbuf);
                        pcdata lexbuf
                )
# 552 "xml_lexer.ml"

  | 4 ->
# 215 "xml_lexer.mll"
                ( Buffer.contents tmp )
# 557 "xml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_pcdata_rec lexbuf __ocaml_lex_state

and entity lexbuf =
    __ocaml_lex_entity_rec lexbuf 40
and __ocaml_lex_entity_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 219 "xml_lexer.mll"
                (
                        let ident = lexeme lexbuf in
                        try
                                Hashtbl.find idents (String.lowercase ident)
                        with
                                Not_found -> "&" ^ ident
                )
# 575 "xml_lexer.ml"

  | 1 ->
# 227 "xml_lexer.mll"
                ( raise (Error EUnterminatedEntity) )
# 580 "xml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_entity_rec lexbuf __ocaml_lex_state

and ident_name lexbuf =
    __ocaml_lex_ident_name_rec lexbuf 45
and __ocaml_lex_ident_name_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 231 "xml_lexer.mll"
                ( lexeme lexbuf )
# 592 "xml_lexer.ml"

  | 1 ->
# 233 "xml_lexer.mll"
                ( error lexbuf EIdentExpected )
# 597 "xml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_ident_name_rec lexbuf __ocaml_lex_state

and close_tag lexbuf =
    __ocaml_lex_close_tag_rec lexbuf 48
and __ocaml_lex_close_tag_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 237 "xml_lexer.mll"
                ( () )
# 609 "xml_lexer.ml"

  | 1 ->
# 239 "xml_lexer.mll"
                ( error lexbuf ECloseExpected )
# 614 "xml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_close_tag_rec lexbuf __ocaml_lex_state

and attributes lexbuf =
    __ocaml_lex_attributes_rec lexbuf 51
and __ocaml_lex_attributes_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 243 "xml_lexer.mll"
                ( [], false )
# 626 "xml_lexer.ml"

  | 1 ->
# 245 "xml_lexer.mll"
                ( [], true )
# 631 "xml_lexer.ml"

  | 2 ->
# 247 "xml_lexer.mll"
                (
                        let key = attribute lexbuf in
                        let data = attribute_data lexbuf in
                        ignore_spaces lexbuf;
                        let others, closed = attributes lexbuf in
                        (key, data) :: others, closed
                )
# 642 "xml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_attributes_rec lexbuf __ocaml_lex_state

and attribute lexbuf =
    __ocaml_lex_attribute_rec lexbuf 55
and __ocaml_lex_attribute_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 257 "xml_lexer.mll"
                ( lexeme lexbuf )
# 654 "xml_lexer.ml"

  | 1 ->
# 259 "xml_lexer.mll"
                ( error lexbuf EAttributeNameExpected )
# 659 "xml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_attribute_rec lexbuf __ocaml_lex_state

and attribute_data lexbuf =
    __ocaml_lex_attribute_data_rec lexbuf 58
and __ocaml_lex_attribute_data_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 263 "xml_lexer.mll"
                (
                        Buffer.reset tmp;
                        last_pos := lexeme_end lexbuf;
                        dq_string lexbuf
                )
# 675 "xml_lexer.ml"

  | 1 ->
# 269 "xml_lexer.mll"
                (
                        Buffer.reset tmp;
                        last_pos := lexeme_end lexbuf;
                        q_string lexbuf
                )
# 684 "xml_lexer.ml"

  | 2 ->
# 275 "xml_lexer.mll"
                ( error lexbuf EAttributeValueExpected )
# 689 "xml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_attribute_data_rec lexbuf __ocaml_lex_state

and dq_string lexbuf =
    __ocaml_lex_dq_string_rec lexbuf 66
and __ocaml_lex_dq_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 279 "xml_lexer.mll"
                ( Buffer.contents tmp )
# 701 "xml_lexer.ml"

  | 1 ->
# 281 "xml_lexer.mll"
                (
                        Buffer.add_char tmp (lexeme_char lexbuf 1);
                        dq_string lexbuf
                )
# 709 "xml_lexer.ml"

  | 2 ->
# 286 "xml_lexer.mll"
                (
                        Buffer.add_string tmp (entity lexbuf);
                        dq_string lexbuf
                )
# 717 "xml_lexer.ml"

  | 3 ->
# 291 "xml_lexer.mll"
                ( raise (Error EUnterminatedString) )
# 722 "xml_lexer.ml"

  | 4 ->
# 293 "xml_lexer.mll"
                (
                        Buffer.add_char tmp (lexeme_char lexbuf 0);
                        dq_string lexbuf
                )
# 730 "xml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_dq_string_rec lexbuf __ocaml_lex_state

and q_string lexbuf =
    __ocaml_lex_q_string_rec lexbuf 73
and __ocaml_lex_q_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 300 "xml_lexer.mll"
                ( Buffer.contents tmp )
# 742 "xml_lexer.ml"

  | 1 ->
# 302 "xml_lexer.mll"
                (
                        Buffer.add_char tmp (lexeme_char lexbuf 1);
                        q_string lexbuf
                )
# 750 "xml_lexer.ml"

  | 2 ->
# 307 "xml_lexer.mll"
                (
                        Buffer.add_string tmp (entity lexbuf);
                        q_string lexbuf
                )
# 758 "xml_lexer.ml"

  | 3 ->
# 312 "xml_lexer.mll"
                ( raise (Error EUnterminatedString) )
# 763 "xml_lexer.ml"

  | 4 ->
# 314 "xml_lexer.mll"
                (
                        Buffer.add_char tmp (lexeme_char lexbuf 0);
                        q_string lexbuf
                )
# 771 "xml_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_q_string_rec lexbuf __ocaml_lex_state

;;

