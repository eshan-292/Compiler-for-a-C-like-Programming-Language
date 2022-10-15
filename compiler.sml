use "ast.sml" ;
structure ASTX :
sig val compile : string -> AST.AST
end =
struct
exception ASTError;
fun compile (fileName) =
    let val inStream =  TextIO.openIn fileName;
        val grab : int -> string = fn
            n => if TextIO.endOfStream inStream
                then ""
                else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
            (msg,line_no,char_no) =>
            print (fileName^"["^Int.toString line_no^":"
                    ^Int.toString char_no^"] "^msg^"\n");
        val (tree,rem) = AST_Parser.parse
         (15,
         (AST_Parser.makeLexer grab fileName),
         printError,
         fileName)
    handle AST_Parser.ParseError => raise ASTError;
val _ = TextIO.closeIn inStream;

in tree
end; 
end