import
  lexer,
  parser

var
  l = initLexer()
  p = initParser()

l.lexFile("testFile.txt")
p.parseTokens(l.data, l.tokens)