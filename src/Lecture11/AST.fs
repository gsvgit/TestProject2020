module AST

type VName = Var of string

type Regexp =
    | RSmb of char
    | RVar of VName
    | Alt of Regexp * Regexp
    | Seq of Regexp * Regexp
    | Opt of Regexp
    | Star of Regexp
    | Intersect of Regexp * Regexp

type Expr =
    | RegExp of Regexp
    | FindAll of string * Regexp
    | IsAcceptable of string * Regexp

type Stmt =
    | Print of VName
    | VDecl of VName * Expr

type Program = List<Stmt>
