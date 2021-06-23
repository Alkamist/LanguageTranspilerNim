import ditto

type
  TokenKind* {.pure.} = enum
    Identifier,
    Number,
    Operator,
    KeyWord,

  Token* = object
    kind*: TokenKind
    start*: int
    finish*: int

proc lineData*(data: string, index: int): tuple[line, character: int] =
  let dataLen = data.len
  var lookIndex = 0

  while lookIndex < dataLen and lookIndex < index:
    if data[lookIndex] == '\n':
      result.character = 0
      result.line += 1
    else:
      result.character += 1

    lookIndex += 1