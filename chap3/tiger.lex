(* For parser interface *)
(* type pos = int *)
(* type lexresult = Tokens.token *)

type svalue = Tokens.svalue
type pos = int
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

(* Begin declarations *)
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commentLevel = ref 0

val stringBuffer : (char list) ref = ref nil
val stringStartPos = ref 0

fun inc (r : int ref) : unit = r := !r + 1

fun dec (r : int ref) : unit = r := !r - 1

fun eof() =
  let
    val pos = hd(!linePos)
    val _ = if !commentLevel <> 0 then
              ErrorMsg.error pos (Int.toString (!commentLevel) ^ " unclosed comments")
            else ()
  in
    Tokens.EOF(pos,pos)
  end

fun parseInt s =
  let
    val pos = hd(!linePos)
    val x = Int.fromString s
            handle Overflow =>
                   (ErrorMsg.error pos ("integer literal too large: " ^ s); SOME ~1)
                 | _ =>
                   (ErrorMsg.error pos ("cannot parse integer: " ^ s); SOME ~1)
  in
    valOf x
  end

%%

%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));

%s COMMENT STRING;
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];

%%

<INITIAL>\n    => (inc lineNum; linePos := yypos :: !linePos; continue());
<INITIAL>{ws}+ => (lex());

<INITIAL>"while"    => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>"for"      => (Tokens.FOR(yypos,yypos+3));
<INITIAL>"to"       => (Tokens.TO(yypos,yypos+2));
<INITIAL>"break"    => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>"let"      => (Tokens.LET(yypos,yypos+3));
<INITIAL>"in"       => (Tokens.IN(yypos,yypos+2));
<INITIAL>"end"      => (Tokens.END(yypos,yypos+3));
<INITIAL>"function" => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>"var"      => (Tokens.VAR(yypos,yypos+3));
<INITIAL>"type"     => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>"array"    => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>"if"       => (Tokens.IF(yypos,yypos+2));
<INITIAL>"then"     => (Tokens.THEN(yypos,yypos+4));
<INITIAL>"else"     => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>"do"       => (Tokens.DO(yypos,yypos+2));
<INITIAL>"of"       => (Tokens.OF(yypos,yypos+2));
<INITIAL>"nil"      => (Tokens.NIL(yypos,yypos+3));

<INITIAL>"+"    => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-"    => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"*"    => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"/"    => (Tokens.DIVIDE(yypos,yypos+1));

<INITIAL>"="    => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"<>"   => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"<"    => (Tokens.LT(yypos,yypos+1));
<INITIAL>"<="   => (Tokens.LE(yypos,yypos+2));
<INITIAL>">"    => (Tokens.GT(yypos,yypos+1));
<INITIAL>">="   => (Tokens.GE(yypos,yypos+2));

<INITIAL>"&"    => (Tokens.AND(yypos,yypos+1));
<INITIAL>"|"    => (Tokens.OR(yypos,yypos+1));

<INITIAL>":="   => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>"."    => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"{"    => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"    => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"["    => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"    => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"("    => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"    => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>";"    => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>":"    => (Tokens.COLON(yypos,yypos+1));
<INITIAL>","    => (Tokens.COMMA(yypos,yypos+1));

<INITIAL>"/*"    => (YYBEGIN COMMENT;
                     inc commentLevel;
                     continue());
<COMMENT>"/*"    => (inc commentLevel;
                     continue());
<COMMENT>"*/"    => (dec commentLevel;
                     if !commentLevel = 0 then YYBEGIN INITIAL else ();
                     continue());
<COMMENT>(.|\n)  => (continue());

<INITIAL>{digit}+ => (Tokens.INT(parseInt(yytext), yypos, yypos + (size yytext)));

<INITIAL>{alpha}({alpha}|{digit}|_)* => (Tokens.ID(yytext, yypos, yypos + (size yytext)));

<INITIAL>\"    => (YYBEGIN STRING;
                   stringStartPos := yypos;
                   stringBuffer := nil; continue());
<STRING>\"     => (YYBEGIN INITIAL;
                   Tokens.STRING(implode (rev (!stringBuffer)), !stringStartPos, yypos+ 1));

<STRING>\\n               => (stringBuffer := #"\n"::(!stringBuffer); continue());
<STRING>\\t               => (stringBuffer := #"\t"::(!stringBuffer); continue());
<STRING>\\\^[@-_]         => (let val n : int = ord (List.nth (explode yytext, 2))
                                  val c = chr (n - 64)
                              in
                                stringBuffer := c::(!stringBuffer)
                              end; continue());
<STRING>\\[0-9]{3}        => (let val n = valOf (Int.fromString (implode (List.drop (explode yytext, 1))))
                                  val c = chr n
                              in
                                stringBuffer := c::(!stringBuffer)
                              end; continue());
<STRING>\\\"              => (stringBuffer := #"\""::(!stringBuffer); continue());
<STRING>\\\\              => (stringBuffer := #"\\"::(!stringBuffer); continue());
<STRING>\\[\ \n\t\f\r]+\\ => (continue());
<STRING>.                 => (stringBuffer := (List.nth (explode yytext, 0))::(!stringBuffer);
                              continue());


<INITIAL>.     => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
