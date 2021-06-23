import
  std/options,
  token,
  ditto

type
  Parser* = object
    data*: string
    tokens*: seq[Token]
    tokenLen: int
    tokenIndex: int
    program: seq[Node]

proc parseNode(s: var Parser): Node

proc initParser*(): Parser =
  result

proc parseError(s: var Parser, msg: string) =
  let (line, character) = lineData(s.data, s.tokens[s.tokenIndex].start)

  raise newException(IOError,
    msg & "\n Line: " & $(line + 1) & "\n Character: " & $(character + 1) & "\n"
  )

proc currentToken(s: Parser): Token =
  s.tokens[s.tokenIndex]

proc tokenText(s: Parser): string =
  let token = s.currentToken
  s.data[token.start..token.finish]

proc increment(s: var Parser) =
  s.tokenIndex += 1
  if s.tokenIndex >= s.tokenLen:
    s.parseError("Unexpectedly ran out of tokens.")

proc tokenIs(s: Parser, kind: TokenKind): bool =
  s.currentToken.kind == kind

proc operator(s: Parser, value: string): bool =
  s.tokenIs(TokenKind.Operator) and s.tokenText == value

proc keyWord(s: Parser, value: string): bool =
  s.tokenIs(TokenKind.KeyWord) and s.tokenText == value

proc parseBinaryExpression(s: var Parser): BinaryExpression =

# proc parseFunctionDefinitionArgument(s: var Parser): FunctionDefinitionArgument =
#   if s.tokenIs(TokenKind.Identifier):
#     result.name = s.tokenText
#     s.increment()

#     if s.tokenIs(TokenKind.Identifier):
#       result.`type` = some(initType(s.tokenText))
#       s.increment()

#     if s.operator("="):
#       s.increment()
#       result.defaultValue = some(s.parseNode())
#       s.increment()

#   else:
#     s.parseError("Invalid function argument name.")

# proc parseFunctionDefinition(s: var Parser): FunctionDefinition =
#   s.increment()

#   if s.tokenIs(TokenKind.Identifier):
#     result.name = s.tokenText
#     s.increment()

#     if s.operator("("):
#       s.increment()

#       while true:
#         result.arguments.add(s.parseFunctionDefinitionArgument())

#         if s.operator(","):
#           s.increment()

#         elif s.operator(")"):
#           s.increment()
#           break

#     if s.tokenIs(TokenKind.Identifier):
#       result.returnType = some(initType(s.tokenText))
#       s.increment()

#     if s.operator("{"):
#       s.increment()

#       while true:
#         result.body.add(s.parseNode())

#         if s.operator("}"):
#           s.increment()
#           break

#   else:
#     s.parseError("Invalid function name.")

proc parseTokens*(s: var Parser, data: string, tokens: seq[Token]) =
  s.data = data
  s.tokens = tokens
  s.tokenLen = s.tokens.len
  s.tokenIndex = 0

  while s.tokenIndex < s.tokenLen:
    s.program.add(s.parseNode())

proc parseNode(s: var Parser): Node =


  # if s.keyWord("fn"):
  #   result = s.parseFunctionDefinition()