import ditto

type
  Lexer* = object
    data*: string
    tokens*: seq[Token]
    dataLen: int
    readIndex: int

proc initLexer*(): Lexer =
  result

proc lexError(s: Lexer, msg: string) =
  let (line, character) = lineData(s.data, s.readIndex)

  raise newException(IOError,
    msg & "\n Line: " & $(line + 1) & "\n Character: " & $(character + 1) & "\n"
  )

proc readToken(s: Lexer, token: Token): string =
  s.data[token.start..token.finish]

proc isInBounds(s: Lexer, lookAhead = 0): bool =
  s.readIndex + lookAhead < s.dataLen

proc readChar(s: Lexer, lookAhead = 0): char =
  s.data[s.readIndex + lookAhead]

proc readText(s: Lexer, lookAheadStart = 0, lookAheadEnd = 0): string =
  let
    start = s.readIndex + lookAheadStart
    finish = s.readIndex + lookAheadEnd
  s.data[start..finish]

proc isWhiteSpace(s: Lexer, lookAhead = 0): bool =
  s.isInBounds(lookAhead) and s.readChar(lookAhead) in WhiteSpace

proc lexIdentifier(s: var Lexer): Token =
  result = Token(kind: TokenKind.Unknown)

  let start = s.readIndex

  if s.isInBounds and s.readChar in IdentStartChars:
    s.readIndex += 1

    while s.isInBounds and s.readChar in IdentChars:
      s.readIndex += 1

    result = Token(
      kind: TokenKind.Identifier,
      start: start,
      finish: s.readIndex - 1,
    )

proc lexString(s: var Lexer): Token =
  result = Token(kind: TokenKind.Unknown)

  let start = s.readIndex

  if s.isInBounds and s.readChar == '"':
    s.readIndex += 1

    while s.isInBounds:
      if s.readChar == '"':
        s.readIndex += 1
        break

      s.readIndex += 1

    if (not s.isInBounds) and (s.readChar(-1) != '"'):
      s.lexError("Unterminated string.")

    result = Token(
      kind: TokenKind.String,
      start: start + 1,
      finish: s.readIndex - 2,
    )

proc lexNumber(s: var Lexer): Token =
  result = Token(kind: TokenKind.Unknown)

  let start = s.readIndex
  var periodCount = 0

  if s.isInBounds and s.readChar in NumberStartChars:
    if s.readChar == '.':
      periodCount += 1
      if (s.isInBounds(1) and s.readChar(1) notin NumberChars) or
         (not s.isInBounds(1)):
        return Token(kind: TokenKind.Unknown)

    s.readIndex += 1

    while s.isInBounds and s.readChar in NumberChars:
      if s.readChar == '.':
        periodCount += 1
        if periodCount > 1:
          return Token(
            kind: TokenKind.Float,
            start: start,
            finish: s.readIndex - 1,
          )

      s.readIndex += 1

    if periodCount < 1:
      return Token(
        kind: TokenKind.Int,
        start: start,
        finish: s.readIndex - 1,
      )

    else:
      if s.isInBounds(0) and
         s.readChar(0) in IdentStartChars:
        s.readIndex -= 1
        return Token(
          kind: TokenKind.Int,
          start: start,
          finish: s.readIndex - 1,
        )
      else:
        return Token(
          kind: TokenKind.Float,
          start: start,
          finish: s.readIndex - 1,
        )

proc lexOperator(s: var Lexer): Token =
  result = Token(kind: TokenKind.Unknown)

  let
    start = s.readIndex
    charsTilEOF = s.dataLen - s.readIndex

  var lookAhead = charsTilEOF - 1

  while lookAhead >= 0:
    if s.readText(0, lookAhead) in Operators:
      s.readIndex += lookAhead + 1
      return Token(
        kind: TokenKind.Operator,
        start: start,
        finish: start + lookAhead,
      )

    lookAhead -= 1

proc lexToken(s: var Lexer): Token =
  result = s.lexString()
  if result.kind == TokenKind.String:
    return result

  result = s.lexNumber()
  if result.kind in [TokenKind.Int, TokenKind.Float]:
    return result

  result = s.lexOperator()
  if result.kind == TokenKind.Operator:
    return result

  result = s.lexIdentifier()
  if result.kind == TokenKind.Identifier:
    if s.readToken(result) in ["true", "false"]:
      result.kind = TokenKind.Bool
    if s.readToken(result) in KeyWords:
      result.kind = TokenKind.KeyWord
    return result

proc lexFile*(s: var Lexer, fileName: string) =
  s.data = readFile(fileName)
  s.dataLen = s.data.len
  s.readIndex = 0

  while s.isInBounds:
    if s.isWhiteSpace: s.readIndex += 1
    else:
      let token = s.lexToken()

      if token.kind == TokenKind.Unknown:
        s.lexError("Unknown token.")

      s.tokens.add(token)

  for token in s.tokens:
    echo $token.kind & ": " & $s.data[token.start..token.finish]