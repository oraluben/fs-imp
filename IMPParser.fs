module IMPParser

open FParsec
open IMPNode

type Stack<'t> =
    | EmptyStack
    | StackNode of Stack<'t> * 't

type State =
    {Program: int
     Label: int
     SharedVar: Set<string>
     LabelStack: Stack<int>
     ProgramStack: Stack<int>
     ExitStack: Stack<int>}
    with
       static member Default = {Program = 0;
                                Label = 0;
                                SharedVar = Set.empty;
                                LabelStack = EmptyStack;
                                ProgramStack = EmptyStack;
                                ExitStack = StackNode(EmptyStack, 0)}

let UpdateAndGetLabel : Parser<Label, State> =
    updateUserState (fun s -> {s with Label = s.Label + 1})
    >>. getUserState |>> (fun s -> LabelID(s.Label))
let UpdateAndPushProgram =
    updateUserState (fun (s : State) -> {s with
                                            ProgramStack = StackNode(s.ProgramStack, s.Program + 1);
                                            Program = s.Program + 1})
let PopAndGetProgram : Parser<ProgramLabel, State> =
    updateUserState (fun s -> {s with ProgramStack =
                                        match s.ProgramStack with
                                        | StackNode(_s, _) | _s -> _s})
    >>. getUserState |>> (fun s -> LabelID(s.Program))

let tryApply x f =
    match f with
    | Some(_f) -> _f x
    | None -> x

let impInt =
    anyOf "012" |>> (string >> int >> Int)
let impName = many1Satisfy isLetter
let impAFactor = impInt <|> (impName |>> Name) |> between spaces spaces
let impATermOp = stringReturn "*" TIMES |> between spaces spaces
let impAExprOp =
    anyOf "+-"
    |>> function
        | '+' -> PLUS
        | '-' | _ -> MINUS

let impAExp, impAExpRef = createParserForwardedToRef<AExp, State>()
let impATerm, impATermRef = createParserForwardedToRef<AExp, State>()
do impATermRef :=
    pipe2   impAFactor
            (opt (pipe2 impATermOp impATerm (fun op a2 -> (fun a1 -> AExpression(a1, op, a2)))))
            tryApply
do impAExpRef :=
    pipe2   impATerm
            (opt (pipe2 impAExprOp impAExp (fun op a2 -> (fun a1 -> AExpression(a1, op, a2)))))
            tryApply


let impBCompareOp =
    (stringReturn "="  EQ)
    <|> (stringReturn "<=" LE)
    |> between spaces spaces
let impBAnd = stringReturn "and" AND |> between spaces spaces
let impBOr = stringReturn "or" OR |> between spaces spaces
let impBool =   (stringReturn "True"  true)
            <|> (stringReturn "False" false) |>> BBool
let impCompare =
    pipe3   impAExp impBCompareOp impAExp
            (fun a1 op a2 -> Compare(a1, op, a2))
    >>= (fun b ->
        updateUserState (fun s -> {s with SharedVar = Set.union s.SharedVar (match s.ProgramStack with
                                                                            | EmptyStack -> Set.empty
                                                                            | StackNode(_) -> b.AVar)})
        >>. preturn b)
let impBFactor = impBool <|> impCompare |> between spaces spaces

let impBExp, impBExpRef = createParserForwardedToRef<BExp, State>()
let impBTerm, impBTermRef = createParserForwardedToRef<BExp, State>()
do impBTermRef :=
    pipe2   impBFactor (opt (pipe2 impBAnd impBExp (fun op a2 -> (fun a1 -> Bitop(a1, op, a2)))))
            tryApply
do impBExpRef :=
    pipe2   impBTerm (opt (pipe2 impBOr impBExp (fun op a2 -> (fun a1 -> Bitop(a1, op, a2)))))
            tryApply


let impCExp, impCExpRef = createParserForwardedToRef<CExp, State>()
let impCTerm, impCTermRef = createParserForwardedToRef<CExp, State>()

let LabelCExp ((c, c_val), (l : Label)) : CExp = c(c_val, l)

let impSkip =
    stringReturn "skip" (Skip, ())
    .>>. UpdateAndGetLabel |>> LabelCExp
let impWait =
    pstring "wait" >>. spaces >>. pstring "(" >>. impBExp .>> pstring ")" |>> (fun b -> Wait, b)
    .>>. UpdateAndGetLabel |>> LabelCExp
let impAssign =
    pipe2   (impName .>> (pstring ":=")) impAExp
            (fun n a -> Assign, (n, a))
    .>>. UpdateAndGetLabel |>> LabelCExp

let impIf =
    pipe3   ((pstring "if") >>. impBExp)
            ((pstring "then") >>. impCExp)
            ((pstring "else") >>. impCExp .>> (pstring "endif"))
            (fun b p1 p2  -> (If, (b, p1, p2)))
    .>>. UpdateAndGetLabel |>> LabelCExp
let impWhile =
    pipe2   impBExp ((pstring "do") >>. impCExp)
            (fun b p -> (While, (b, p)))
    |> between (pstring "while") (pstring "endwhile")
    .>>. UpdateAndGetLabel |>> LabelCExp
let impCo =
    pipe2   (UpdateAndPushProgram >>. impCExp .>>. (PopAndGetProgram .>>. UpdateAndGetLabel) |>> Program)
            ((pstring "||") >>. UpdateAndPushProgram >>. impCExp .>>. (PopAndGetProgram .>>. UpdateAndGetLabel) |>> Program)
            (fun p1 p2 -> (Co, (p1, p2)))
    |> between (pstring "cobegin") (pstring "coend")
    .>>. UpdateAndGetLabel |>> LabelCExp
let impSequence =
    attempt (pipe2  impCTerm
                    ((pstring ";") >>. impCExp)
                    (fun p1 p2 -> Sequence((p1, p2), p1.Label)))
do impCTermRef :=
    impWhile <|> impIf <|> impCo <|> impSkip <|> impWait <|> impAssign
     |> between spaces spaces
do impCExpRef :=
    (impSequence <|> impCTerm)


let impProgram = impCExp .>>. UpdateAndGetLabel |>> (fun (c, l) -> Program(c, (LabelID(0), l))) .>> eof