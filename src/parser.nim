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

proc parseNode(s: var Parser, toIndex = none(int)): Node

proc initParser*(): Parser =
  result.program = Node(
    kind: NodeKind.List,
    list: List(kind: ListKind.Statement),
  )

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
  result = initNone()
  if s.tokenIs(TokenKind.Identifier):
    result = initIdentifier(s.tokenText)
    s.readIndex += 1

proc parseInt(s: var Parser): Node =
  result = initNone()

  if s.tokenIs(TokenKind.Int):
    var intValue = 0

    if parseInt(s.tokenText, intValue) == 0:
      return initNone()

    result = initIntLiteral(intValue)
    s.readIndex += 1

proc parseFloat(s: var Parser): Node =
  result = initNone()

  if s.tokenIs(TokenKind.Float):
    var floatValue = 0.0

    if parseFloat(s.tokenText, floatValue) == 0:
      return initNone()

    result = initFloatLiteral(floatValue)
    s.readIndex += 1

proc parseBool(s: var Parser): Node =
  result = initNone()

  if s.tokenIs(TokenKind.Bool):
    let text = s.tokenText
    var boolValue = false

    if text == "true":
      boolValue = true
    elif text == "false":
      boolValue = false
    else:
      s.parseError("Invalid boolean value.")

    result = initBoolLiteral(boolValue)
    s.readIndex += 1

proc parseString(s: var Parser): Node =
  result = initNone()
  if s.tokenIs(TokenKind.String):
    result = initStringLiteral(s.tokenText)
    s.readIndex += 1

proc parseUnaryExpression(s: var Parser): Node =
  if s.operator("-"):
    s.readIndex += 1
    result = initUnaryExpression(UnaryExpressionKind.Minus, s.parseUnaryExpression())
    if result.expression.unary.value.kind != NodeKind.None: return result
  else:
    result = initUnaryExpression(UnaryExpressionKind.Plus)

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

  return initNone()

proc parseBinaryExpressions(s: var Parser, kinds: openarray[BinaryExpressionKind], toIndex = none(int)): Node =
  result = initNone()

  let startOfEntireExpression = s.readIndex

  while (toIndex.isNone and s.isInBounds and not s.operator(";")) or
        (toIndex.isSome and s.readIndex <= toIndex.get):
    let startOfThisExpression = s.readIndex

    var left = s.parseUnaryExpression()

    if left.kind == NodeKind.Expression and
       left.expression.kind == ExpressionKind.Unary and
       s.tokenIs(TokenKind.Operator):

      let opKind = s.tokenText.toBinaryExpressionKind

      if opKind.isSome and opKind.get in kinds:
        result = initBinaryExpression(opKind.get)

        s.readIndex = startOfEntireExpression
        result.expression.binary.left = s.parseNode(some(startOfThisExpression - 1))

        s.readIndex += 1
        result.expression.binary.right = s.parseNode()

        if result.expression.binary.left.kind == NodeKind.None or
           result.expression.binary.right.kind == NodeKind.None:
          s.readIndex = startOfEntireExpression
          return initNone()
        else:
          return result
      else:
        s.readIndex = startOfEntireExpression
        return initNone()

    s.readIndex += 1

  if result.kind == NodeKind.None:
    s.readIndex = startOfEntireExpression

proc parseBinaryExpression(s: var Parser, toIndex = none(int)): Node =
  let startOfEntireExpression = s.readIndex

  result = s.parseBinaryExpressions(
    [BinaryExpressionKind.Equals,
     BinaryExpressionKind.PlusEquals, BinaryExpressionKind.MinusEquals,
     BinaryExpressionKind.StarEquals, BinaryExpressionKind.SlashEquals,
     BinaryExpressionKind.ModEquals],
    toIndex
  )
  if result.kind != NodeKind.None: return result

  s.readIndex = startOfEntireExpression
  result = s.parseBinaryExpressions([BinaryExpressionKind.Or], toIndex)
  if result.kind != NodeKind.None: return result

  s.readIndex = startOfEntireExpression
  result = s.parseBinaryExpressions([BinaryExpressionKind.And], toIndex)
  if result.kind != NodeKind.None: return result

  s.readIndex = startOfEntireExpression
  result = s.parseBinaryExpressions(
    [BinaryExpressionKind.EqualsEquals, BinaryExpressionKind.BangEquals,
     BinaryExpressionKind.Lesser, BinaryExpressionKind.LesserEquals,
     BinaryExpressionKind.Greater, BinaryExpressionKind.GreaterEquals],
    toIndex
  )
  if result.kind != NodeKind.None: return result

  s.readIndex = startOfEntireExpression
  result = s.parseBinaryExpressions(
    [BinaryExpressionKind.Plus, BinaryExpressionKind.Minus],
    toIndex
  )
  if result.kind != NodeKind.None: return result

  s.readIndex = startOfEntireExpression
  result = s.parseBinaryExpressions(
    [BinaryExpressionKind.Star, BinaryExpressionKind.Slash,
     BinaryExpressionKind.Mod],
    toIndex
  )
  if result.kind != NodeKind.None: return result

proc parseStatementList(s: var Parser): Node =
  result = initStatementList()
  while s.isInBounds and not s.operator("}"):
    result.list.children.add(s.parseNode())
    s.readIndex += 1

proc parseTokens*(s: var Parser, data: string, tokens: seq[Token]) =
  s.data = data
  s.tokens = tokens
  s.tokenLen = s.tokens.len
  s.readIndex = 0
  s.program = s.parseStatementList()
  echo s.program.toDitto

proc parseNode(s: var Parser, toIndex = none(int)): Node =
  if not s.isInBounds: return Node(kind: NodeKind.None)

  result = s.parseBinaryExpression(toIndex)
  if result.kind != NodeKind.None: return result

  result = s.parseUnaryExpression()
  if result.kind != NodeKind.None: return result