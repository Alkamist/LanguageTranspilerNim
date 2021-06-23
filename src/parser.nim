import
  std/parseutils,
  ditto

type
  Parser* = object
    data*: string
    tokens*: seq[Token]
    tokenLen: int
    readIndex: int
    program: Node

proc parseNode(s: var Parser): Node

proc initParser*(): Parser =
  result

proc parseError(s: var Parser, msg: string) =
  let
    index = s.readIndex.min(s.tokenLen - 1)
    (line, character) = lineData(s.data, s.tokens[index].start)

  raise newException(IOError,
    msg & "\n Line: " & $(line + 1) & "\n Character: " & $(character + 1) & "\n"
  )

proc isInBounds(s: Parser, lookAhead = 0): bool =
  s.readIndex + lookAhead < s.tokenLen

proc tokenText(s: Parser, lookAhead = 0): string =
  let
    index = s.readIndex + lookAhead
    token = s.tokens[index]
  s.data[token.start..token.finish]

proc tokenIs(s: Parser, kind: TokenKind, lookAhead = 0): bool =
  let index = s.readIndex + lookAhead
  index < s.tokenLen and s.tokens[index].kind == kind

proc operator(s: Parser, value: string, lookAhead = 0): bool =
  s.tokenIs(TokenKind.Operator, lookAhead) and s.tokenText(lookAhead) == value

proc keyWord(s: Parser, value: string, lookAhead = 0): bool =
  s.tokenIs(TokenKind.KeyWord, lookAhead) and s.tokenText(lookAhead) == value

proc parseIdentifier(s: var Parser): Node =
  result = Node(kind: NodeKind.None)

  if s.tokenIs(TokenKind.Identifier):
    result = Node(
      kind: NodeKind.Identifier,
      identifierName: s.tokenText,
    )

    s.readIndex += 1

proc parseFloat(s: var Parser): Node =
  result = Node(kind: NodeKind.None)

  if s.tokenIs(TokenKind.Int):
    var intValue = 0

    if parseInt(s.tokenText, intValue) == 0:
      return Node(kind: NodeKind.None)

    result = Node(
      kind: NodeKind.Literal,
      literalKind: LiteralKind.Int,
      literalIntValue: intValue,
    )

    s.readIndex += 1

proc parseInt(s: var Parser): Node =
  result = Node(kind: NodeKind.None)

  if s.tokenIs(TokenKind.Float):
    var floatValue = 0.0

    if parseFloat(s.tokenText, floatValue) == 0:
      return Node(kind: NodeKind.None)

    result = Node(
      kind: NodeKind.Literal,
      literalKind: LiteralKind.Float,
      literalFloatValue: floatValue,
    )

    s.readIndex += 1

proc parseBool(s: var Parser): Node =
  result = Node(kind: NodeKind.None)

  if s.tokenIs(TokenKind.Bool):
    let text = s.tokenText
    var boolValue = false

    if text == "true":
      boolValue = true
    elif text == "false":
      boolValue = false
    else:
      s.parseError("Invalid boolean value.")

    result = Node(
      kind: NodeKind.Literal,
      literalKind: LiteralKind.Bool,
      literalBoolValue: boolValue,
    )

    s.readIndex += 1

proc parseString(s: var Parser): Node =
  result = Node(kind: NodeKind.None)

  if s.tokenIs(TokenKind.String):
    result = Node(
      kind: NodeKind.Literal,
      literalKind: LiteralKind.String,
      literalStringValue: s.tokenText,
    )

    s.readIndex += 1

proc parseUnaryExpression(s: var Parser): Node =
  var unaryKind = UnaryExpressionKind.Plus

  if s.operator("-"):
    s.readIndex += 1
    unaryKind = UnaryExpressionKind.Minus

  result = Node(
    kind: NodeKind.Expression,
    expressionKind: ExpressionKind.Unary,
    expressionUnaryKind: unaryKind,
  )

  result.expressionUnaryValue = s.parseString()
  if result.expressionUnaryValue.kind != NodeKind.None: return result

  result.expressionUnaryValue = s.parseBool()
  if result.expressionUnaryValue.kind != NodeKind.None: return result

  result.expressionUnaryValue = s.parseFloat()
  if result.expressionUnaryValue.kind != NodeKind.None: return result

  result.expressionUnaryValue = s.parseInt()
  if result.expressionUnaryValue.kind != NodeKind.None: return result

  result.expressionUnaryValue = s.parseIdentifier()
  if result.expressionUnaryValue.kind != NodeKind.None: return result

  result = Node(kind: NodeKind.None)

proc parseEquals(s: var Parser): Node =
  result = Node(kind: NodeKind.None)

  if s.tokenIs(TokenKind.Identifier) and s.operator("=", 1):
    result = Node(
      kind: NodeKind.Expression,
      expressionKind: ExpressionKind.Binary,
      expressionBinaryKind: BinaryExpressionKind.Equals,
    )

    result.expressionBinaryLeft = s.parseUnaryExpression()
    s.readIndex += 2
    result.expressionBinaryRight = s.parseNode()

proc parseTokens*(s: var Parser, data: string, tokens: seq[Token]) =
  s.data = data
  s.tokens = tokens
  s.tokenLen = s.tokens.len
  s.readIndex = 0
  s.program = s.parseNode()

  #echo s.program.expressionKind
  #echo s.program.expressionUnaryKind
  #echo s.program.expressionUnaryValue.kind
  #echo s.program.expressionUnaryValue.literalKind
  #echo s.program.expressionUnaryValue.literalIntValue

proc parseNode(s: var Parser): Node =
  if not s.isInBounds: return Node(kind: NodeKind.None)

  result = s.parseEquals()
  if result.kind != NodeKind.None: return result

  result = s.parseUnaryExpression()
  if result.kind != NodeKind.None: return result