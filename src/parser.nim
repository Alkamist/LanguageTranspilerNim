import
  token,
  ditto

type
  Parser* = object
    data*: string
    tokens*: seq[Token]
    tokenLen: int
    tokenStart: int
    tokenLook: int
    syntaxTree: Node

proc currentToken(s: Parser): Token =
  s.tokens[s.tokenStart + s.tokenLook]

proc parseError(s: var Parser, msg: string) =
  let (line, character) = lineData(s.data, s.currentToken.start)

  raise newException(IOError,
    msg & "\n Line: " & $(line + 1) & "\n Character: " & $(character + 1) & "\n"
  )

proc readToken(s: Parser, token: Token): string =
  s.data[token.start..token.finish]

proc checkFunctionDefinition(s: Parser): bool =
  s.currentToken.kind == TokenKind.KeyWord and
  s.readToken(s.currentToken) == "fn"

proc parseFunctionDefinition(s: var Parser) =
  s.tokenLook += 1

  while s.tokenStart + s.tokenLook < s.tokenLen:
    case s.tokenLook:
    of 1:
      if s.currentToken.kind == TokenKind.Identifier:
        discard
      else:
        s.parseError("Invalid function name.")
    else:
      discard

    s.tokenLook += 1

proc parseTokens*(s: var Parser, data: string, tokens: seq[Token]) =
  s.data = data
  s.tokens = tokens
  s.tokenLen = s.tokens.len
  s.tokenStart = 0
  s.tokenLook = 0

  while s.tokenStart < s.tokenLen:
    if s.checkFunctionDefinition():
      s.parseFunctionDefinition()

    s.tokenStart += 1
    s.tokenLook = 0