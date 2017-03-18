// Signature file for parser generated by fsyacc
module IMPParser
type token = 
  | EOF
  | CO
  | COBEGIN
  | COEND
  | SKIP
  | WAIT
  | LPAREN
  | RPAREN
  | IF
  | THEN
  | ELSE
  | ENDIF
  | WHILE
  | DO
  | ENDWHILE
  | ASSIGN
  | AND
  | OR
  | NOT
  | EQ
  | LE
  | PLUS
  | MINUS
  | TIMES
  | TRUE
  | FALSE
  | SEMI
  | INT of (int)
  | NAME of (string)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_CO
    | TOKEN_COBEGIN
    | TOKEN_COEND
    | TOKEN_SKIP
    | TOKEN_WAIT
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_ENDIF
    | TOKEN_WHILE
    | TOKEN_DO
    | TOKEN_ENDWHILE
    | TOKEN_ASSIGN
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_NOT
    | TOKEN_EQ
    | TOKEN_LE
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_SEMI
    | TOKEN_INT
    | TOKEN_NAME
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_a_exp
    | NONTERM_b_exp
    | NONTERM_c_exp
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (IMPNode.Program) 
