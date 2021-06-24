import
  std/parseutils,
  std/options,
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
      identifier: Identifier(name: s.tokenText),
    )

    s.readIndex += 1

proc parseInt(s: var Parser): Node =
  result = Node(kind: NodeKind.None)

  if s.tokenIs(TokenKind.Int):
    var intValue = 0

    if parseInt(s.tokenText, intValue) == 0:
      return Node(kind: NodeKind.None)

    result = Node(
      kind: NodeKind.Literal,
      literal: Literal(
        kind: LiteralKind.Int,
        intValue: intValue,
      ),
    )

    s.readIndex += 1

proc parseFloat(s: var Parser): Node =
  result = Node(kind: NodeKind.None)

  if s.tokenIs(TokenKind.Float):
    var floatValue = 0.0

    if parseFloat(s.tokenText, floatValue) == 0:
      return Node(kind: NodeKind.None)

    result = Node(
      kind: NodeKind.Literal,
      literal: Literal(
        kind: LiteralKind.Float,
        floatValue: floatValue,
      ),
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
      literal: Literal(
        kind: LiteralKind.Bool,
        boolValue: boolValue,
      ),
    )

    s.readIndex += 1

proc parseString(s: var Parser): Node =
  result = Node(kind: NodeKind.None)

  if s.tokenIs(TokenKind.String):
    result = Node(
      kind: NodeKind.Literal,
      literal: Literal(
        kind: LiteralKind.String,
        stringValue: s.tokenText,
      ),
    )

    s.readIndex += 1

proc parseUnaryExpression(s: var Parser): Node =
  var unaryKind = UnaryExpressionKind.Plus

  if s.operator("-"):
    s.readIndex += 1
    unaryKind = UnaryExpressionKind.Minus

  result = Node(
    kind: NodeKind.Expression,
    expression: Expression(
      kind: ExpressionKind.Unary,
      unary: UnaryExpression(kind: unaryKind)
    ),
  )

  result.expression.unary.value = s.parseString()
  if result.expression.unary.value.kind != NodeKind.None: return result

  result.expression.unary.value = s.parseBool()
  if result.expression.unary.value.kind != NodeKind.None: return result

  result.expression.unary.value = s.parseFloat()
  if result.expression.unary.value.kind != NodeKind.None: return result

  result.expression.unary.value = s.parseInt()
  if result.expression.unary.value.kind != NodeKind.None: return result

  result.expression.unary.value = s.parseIdentifier()
  if result.expression.unary.value.kind != NodeKind.None: return result

  result = Node(kind: NodeKind.None)

proc parseBinaryExpressionLeftToRightOfKinds(s: var Parser, kinds: openarray[BinaryExpressionKind]): Node =
  result = Node(kind: NodeKind.None)

  #while s.isInBounds and not s.operator(";"):
  var left = s.parseUnaryExpression()

  if left.kind == NodeKind.Expression and
     left.expression.kind == ExpressionKind.Unary and
     s.tokenIs(TokenKind.Operator):

    let kind = s.tokenText.toBinaryExpressionKind

    if kind.isSome and kind.get in kinds:
      result = Node(
        kind: NodeKind.Expression,
        expression: Expression(
          kind: ExpressionKind.Binary,
          binary: BinaryExpression(
            kind: kind.get,
            left: left,
          ),
        ),
      )

      s.readIndex += 1
      result.expression.binary.right = s.parseNode()

proc parseBinaryExpression(s: var Parser): Node =
  var start = s.readIndex

  result = s.parseBinaryExpressionLeftToRightOfKinds([BinaryExpressionKind.Star, BinaryExpressionKind.Slash])
  if result.kind != NodeKind.None: return result

  result = s.parseBinaryExpressionLeftToRightOfKinds([BinaryExpressionKind.Plus, BinaryExpressionKind.Minus])
  if result.kind != NodeKind.None: return result

proc parseTokens*(s: var Parser, data: string, tokens: seq[Token]) =
  s.data = data
  s.tokens = tokens
  s.tokenLen = s.tokens.len
  s.readIndex = 0
  s.program = s.parseNode()

  echo s.program.kind
  echo s.program.expression.kind
  echo s.program.expression.binary.kind

proc parseNode(s: var Parser): Node =
  if not s.isInBounds: return Node(kind: NodeKind.None)

  result = s.parseBinaryExpression()
  if result.kind != NodeKind.None: return result

  result = s.parseUnaryExpression()
  if result.kind != NodeKind.None: return result