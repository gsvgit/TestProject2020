module RegexpParser

open System.IO
open FSharp.Text.Lexing
let parseRegexpFromString text =
    let lexbuf = LexBuffer<char>.FromString text
    let regexp = CalcParser.start CalcLexer.tokenStream lexbuf
    regexp
