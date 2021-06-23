import
  token,
  ditto

type
  Lexer* = object
    data*: string
    tokens*: seq[Token]
    dataLen: int
    charStart: int
    charFinish: int

proc initLexer*(): Lexer =
  result

proc lexError(s: var Lexer, msg: string) =
  let (line, character) = lineData(s.data, s.charStart)

  raise newException(IOError,
    msg & "\n Line: " & $(line + 1) & "\n Character: " & $(character + 1) & "\n"
  )

proc readTo(s: Lexer, index: int): string =
  s.data[s.charStart..index]

proc addToken(s: var Lexer, kind: TokenKind) =
  s.tokens.add(Token(
    kind: kind,
    start: s.charStart,
    finish: s.charFinish,
  ))

proc checkNewLine(s: var Lexer): bool =
  s.data[s.charStart] == '\n'

proc checkWhiteSpace(s: Lexer): bool =
  s.charStart < s.dataLen and s.data[s.charStart] in WhiteSpace

proc checkIdentifier(s: var Lexer): bool =
  var look = s.charStart

  if look < s.dataLen and s.data[look] in IdentStartChars:
    look += 1

    while look < s.dataLen and s.data[look] in IdentChars:
      look += 1

    s.charFinish = look - 1

    result = true

proc checkKeyWord(s: var Lexer): bool =
  s.checkIdentifier() and s.readTo(s.charFinish) in KeyWords

proc checkNumber(s: var Lexer): bool =
  var
    look = s.charStart
    periodCount = 0

  if look < s.dataLen and s.data[look] in NumberStartChars:
    if s.data[look] == '.':
      periodCount += 1
      let lookOneMore = look + 1
      if lookOneMore < s.dataLen and s.data[lookOneMore] notin NumberChars:
        s.charFinish = look
        return false

    look += 1

    while look < s.dataLen and s.data[look] in NumberChars:
      if s.data[look] == '.':
        periodCount += 1
        if periodCount > 1:
          s.charFinish = look - 1
          return true

      look += 1

    s.charFinish = look - 1
    return true

proc checkOperator(s: var Lexer): bool =
  let charsTilEOF = s.dataLen - s.charStart
  var look = s.charStart + min(MaxOperatorLength - 1, charsTilEOF - 1)

  while look >= s.charStart:
    if s.readTo(look) in Operators:
      s.charFinish = look
      return true

    look -= 1

proc lexFile*(s: var Lexer, fileName: string) =
  s.data = readFile(fileName)
  s.dataLen = s.data.len
  s.charStart = 0
  s.charFinish = 0

  while s.charStart < s.dataLen:
    if s.checkNewLine(): discard
    elif s.checkWhiteSpace(): discard
    elif s.checkKeyWord(): s.addToken(TokenKind.KeyWord)
    elif s.checkIdentifier(): s.addToken(TokenKind.Identifier)
    elif s.checkNumber(): s.addToken(TokenKind.Number)
    elif s.checkOperator(): s.addToken(TokenKind.Operator)
    else: s.lexError("Unknown token.")

    s.charStart = s.charFinish + 1
    s.charFinish = s.charStart

  # for token in s.tokens:
  #   echo $token.kind & ": " & $s.data[token.start..token.finish]