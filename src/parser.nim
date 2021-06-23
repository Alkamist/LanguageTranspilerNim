import ditto

type
  Parser* = object
    data*: string
    tokens*: seq[Token]
    tokenLen: int
    tokenIndex: int
    program: Node

proc parseNode(s: var Parser): Node

proc initParser*(): Parser =
  result

proc parseError(s: var Parser, msg: string) =
  let
    index = s.tokenIndex.min(s.tokenLen - 1)
    (line, character) = lineData(s.data, s.tokens[index].start)

  raise newException(IOError,
    msg & "\n Line: " & $(line + 1) & "\n Character: " & $(character + 1) & "\n"
  )

proc isAtEnd(s: Parser): bool =
  s.tokenIndex >= s.tokenLen

proc tokenText(s: Parser, lookAhead = 0): string =
  let
    index = s.tokenIndex + lookAhead
    token = s.tokens[index]
  s.data[token.start..token.finish]

proc tokenIs(s: Parser, kind: TokenKind, lookAhead = 0): bool =
  let index = s.tokenIndex + lookAhead
  index < s.tokenLen and s.tokens[index].kind == kind

proc operator(s: Parser, value: string, lookAhead = 0): bool =
  s.tokenIs(TokenKind.Operator, lookAhead) and s.tokenText(lookAhead) == value

proc keyWord(s: Parser, value: string, lookAhead = 0): bool =
  s.tokenIs(TokenKind.KeyWord, lookAhead) and s.tokenText(lookAhead) == value

proc parseIdentifier(s: var Parser): Node =
  result = Node(kind: NodeKind.None)

  if s.tokenIs(TokenKind.Identifier):
    let text = s.tokenText

    if text in ["true", "false"]:
      result = Node(
        kind: NodeKind.Literal,
        literalKind: LiteralKind.Bool,
      )

    else:
      result = Node(
        kind: NodeKind.Identifier,
        identifierName: s.tokenText,
      )

    s.tokenIndex += 1

proc parseNumber(s: var Parser): Node =
  result = Node(kind: NodeKind.None)

  if s.tokenIs(TokenKind.Number):
    let text = s.tokenText

    # result = Node(
    #   kind: NodeKind.Literal,
    #   literalKind: TypeKind.Number,
    #   typeName: identifier
    # )

    s.tokenIndex += 1

#proc parseStatement(s: var Parser): Node =


proc parseEquals(s: var Parser): Node =
  result = Node(kind: NodeKind.None)

  if s.tokenIs(TokenKind.Identifier) and s.operator("=", 1):
    result = Node(
      kind: NodeKind.Expression,
      expressionKind: ExpressionKind.Binary,
      expressionBinaryKind: BinaryExpressionKind.Equals,
    )

    result.expressionBinaryLeft = s.parseIdentifier()
    s.tokenIndex += 2
    result.expressionBinaryRight = s.parseNode()

proc parsePlus(s: var Parser): Node =
  result = Node(kind: NodeKind.None)

  if s.tokenIs(TokenKind.Identifier) and s.operator("+", 1):
    result = Node(
      kind: NodeKind.Expression,
      expressionKind: ExpressionKind.Binary,
      expressionBinaryKind: BinaryExpressionKind.Plus,
    )

    result.expressionBinaryLeft = s.parseIdentifier()
    s.tokenIndex += 2
    result.expressionBinaryRight = s.parseNode()

proc parseTokens*(s: var Parser, data: string, tokens: seq[Token]) =
  s.data = data
  s.tokens = tokens
  s.tokenLen = s.tokens.len
  s.tokenIndex = 0
  s.program = s.parseNode()

proc parseNode(s: var Parser): Node =
  if s.isAtEnd: return Node(kind: NodeKind.None)

  result = s.parseEquals()
  if result.kind != NodeKind.None: return result

  result = s.parseIdentifier()
  if result.kind != NodeKind.None: return result