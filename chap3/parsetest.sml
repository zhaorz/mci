structure Parse :
          sig
            val parse : string -> unit
            val parseDir : string -> unit
          end =
struct

structure FS = OS.FileSys
structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
structure Lex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
structure TigerP = Join(structure ParserData = TigerLrVals.ParserData
                        structure Lex=Lex
                        structure LrParser = LrParser)

fun parse filename =
  let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
      val file = TextIO.openIn filename
      fun get _ = TextIO.input file
      fun parseerror(s,p1,p2) = ErrorMsg.error p1 s
      val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
      val (absyn, _) = TigerP.parse(30,lexer,parseerror,())
  in TextIO.closeIn file;
     absyn
  end handle LrParser.ParseError => raise ErrorMsg.Error


fun parseDir dirpath =
  let
    val ds : FS.dirstream = FS.openDir dirpath

    fun fullPath filename = dirpath ^ "/" ^ filename

    fun parseDirStream ds =
      case FS.readDir ds of
          NONE => ()
        | SOME filename =>
          let
            val _ = print ("parsing " ^ filename ^ "\n")
            val _ = parse (fullPath filename)
          in
            parseDirStream ds
          end
  in
    parseDirStream ds
  end

end
