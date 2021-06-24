import std/options

type
  TokenKind* {.pure.} = enum
    Unknown,
    Identifier,
    Float,
    Int,
    Bool,
    String,
    Operator,
    KeyWord,

  Token* = object
    kind*: TokenKind
    start*: int
    finish*: int

  Identifier* = object
    name*: string

  TypeKind* {.pure.} = enum
    Int,
    Float,
    String,
    Bool,
    Custom,

  Type* = object
    case kind*: TypeKind
    of TypeKind.Int, TypeKind.Float, TypeKind.String, TypeKind.Bool:
      discard
    of TypeKind.Custom:
      name*: string

  LiteralKind* {.pure.} = enum
    Int,
    Float,
    String,
    Bool,

  Literal* = object
    case kind*: LiteralKind
    of LiteralKind.Int:
      intValue*: int
    of LiteralKind.Float:
      floatValue*: float
    of LiteralKind.String:
      stringValue*: string
    of LiteralKind.Bool:
      boolValue*: bool

  ListKind* {.pure.} = enum
    Statement,
    Comma,

  List* = object
    kind*: ListKind
    children*: seq[Node]

  UnaryExpressionKind* {.pure.} = enum
    Plus,
    Minus,

  UnaryExpression* = object
    kind*: UnaryExpressionKind
    value*: Node

  BinaryExpressionKind* {.pure.} = enum
    Equals,
    EqualsEquals,
    Greater,
    GreaterEquals,
    Lesser,
    LesserEquals,
    Plus,
    PlusEquals,
    Minus,
    MinusEquals,
    Star,
    StarEquals,
    Slash,
    SlashEquals,

  BinaryExpression* = object
    kind*: BinaryExpressionKind
    left*: Node
    right*: Node

  ExpressionKind* {.pure.} = enum
    Unary,
    Binary,

  Expression* = object
    case kind*: ExpressionKind
    of ExpressionKind.Unary: unary*: UnaryExpression
    of ExpressionKind.Binary: binary*: BinaryExpression

  # FunctionDefinitionArgument* = object
  #   name*: string
  #   `type`*: Node
  #   defaultValue*: Node

  # FunctionDefinition* = object
  #   name*: string
  #   arguments*: Node
  #   returnType*: Node
  #   body*: Node

  NodeKind* {.pure.} = enum
    None,
    List,
    Identifier,
    Type,
    Literal,
    Expression,
    # FunctionDefinitionArgument,
    # FunctionDefinition,

  Node* = ref NodeObject

  NodeObject* = object
    case kind*: NodeKind
    of NodeKind.None: discard
    of NodeKind.List: list*: List
    of NodeKind.Identifier: identifier*: Identifier
    of NodeKind.Type: `type`*: Type
    of NodeKind.Literal: literal*: Literal
    of NodeKind.Expression: expression*: Expression
    # of NodeKind.FunctionDefinitionArgument: functionDefinitionArgument*: FunctionDefinitionArgument
    # of NodeKind.FunctionDefinition: functionDefinition*: FunctionDefinition

const
  KeyWords* = ["var", "const", "if", "else", "elif", "for", "while", "fn"]
  BuiltinTypes* = ["int", "float", "string", "bool"]
  WhiteSpace* = {' ', '\t', '\v', '\c', '\n', '\f'}
  IdentStartChars* = {'a'..'z', 'A'..'Z', '_'}
  IdentChars* = {'a'..'z', 'A'..'Z', '0'..'9', '_'}
  NumberStartChars* = {'0'..'9', '.'}
  NumberChars* = {'0'..'9', '_', '.'}
  Operators* = ["+", "+=", "-", "-=", "*", "*=", "/", "/=", "<",
                "<=", ">", ">=", "=", "==", ".", "(", ")", "{",
                "}", "[", "]", ",", ":", ";"]
  MaxOperatorLength* = block:
    var biggest = 0
    for operator in Operators:
      biggest = max(biggest, operator.len)
    biggest

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

proc toOperatorString*(binaryExpressionKind: BinaryExpressionKind): string =
  case binaryExpressionKind:
  of BinaryExpressionKind.Equals: "="
  of BinaryExpressionKind.EqualsEquals: "=="
  of BinaryExpressionKind.Greater: ">"
  of BinaryExpressionKind.GreaterEquals: ">="
  of BinaryExpressionKind.Lesser: "<"
  of BinaryExpressionKind.LesserEquals: "<="
  of BinaryExpressionKind.Plus: "+"
  of BinaryExpressionKind.PlusEquals: "+="
  of BinaryExpressionKind.Minus: "-"
  of BinaryExpressionKind.MinusEquals: "-="
  of BinaryExpressionKind.Star: "*"
  of BinaryExpressionKind.StarEquals: "*="
  of BinaryExpressionKind.Slash: "/"
  of BinaryExpressionKind.SlashEquals: "/="

proc toBinaryExpressionKind*(text: string): Option[BinaryExpressionKind] =
  case text:
  of "=": some(BinaryExpressionKind.Equals)
  of "==": some(BinaryExpressionKind.EqualsEquals)
  of ">": some(BinaryExpressionKind.Greater)
  of ">=": some(BinaryExpressionKind.GreaterEquals)
  of "<": some(BinaryExpressionKind.Lesser)
  of "<=": some(BinaryExpressionKind.LesserEquals)
  of "+": some(BinaryExpressionKind.Plus)
  of "+=": some(BinaryExpressionKind.PlusEquals)
  of "-": some(BinaryExpressionKind.Minus)
  of "-=": some(BinaryExpressionKind.MinusEquals)
  of "*": some(BinaryExpressionKind.Star)
  of "*=": some(BinaryExpressionKind.StarEquals)
  of "/": some(BinaryExpressionKind.Slash)
  of "/=": some(BinaryExpressionKind.SlashEquals)
  else: none(BinaryExpressionKind)