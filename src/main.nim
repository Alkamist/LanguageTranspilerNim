import
  lexer,
  parser

var
  l = Lexer()
  p = Parser()

l.lexFile("testFile.txt")
p.parseTokens(l.data, l.tokens)
