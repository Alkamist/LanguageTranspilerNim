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

  TypeKind* {.pure.} = enum
    Int,
    Float,
    String,
    Bool,
    Custom,

  LiteralKind* {.pure.} = enum
    Int,
    Float,
    String,
    Bool,

  ListKind* {.pure.} = enum
    Statement,
    Comma,

  ExpressionKind* {.pure.} = enum
    Unary,
    Binary,

  UnaryExpressionKind* {.pure.} = enum
    Plus,
    Minus,

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

  NodeKind* {.pure.} = enum
    None,
    List,
    Identifier,
    Type,
    Literal,
    Expression,
    FunctionDefinitionArgument,
    FunctionDefinition,

  Node* = ref NodeObject

  NodeObject* = object
    case kind*: NodeKind

    of NodeKind.None:
      discard

    of NodeKind.List:
      listKind*: ListKind
      listChildren*: seq[Node]

    of NodeKind.Identifier:
      identifierName*: string

    of NodeKind.Type:
      case typeKind*: TypeKind
      of TypeKind.Int, TypeKind.Float, TypeKind.String, TypeKind.Bool:
        discard
      of TypeKind.Custom: typeName*:
        string

    of NodeKind.Literal:
      case literalKind*: LiteralKind
      of LiteralKind.Int:
        literalIntValue*: int
      of LiteralKind.Float:
        literalFloatValue*: float
      of LiteralKind.String:
        literalStringValue*: string
      of LiteralKind.Bool:
        literalBoolValue*: bool

    of NodeKind.Expression:
      case expressionKind*: ExpressionKind
      of ExpressionKind.Unary:
        expressionUnaryKind*: UnaryExpressionKind
        expressionUnaryValue*: Node
      of ExpressionKind.Binary:
        expressionBinaryKind*: BinaryExpressionKind
        expressionBinaryLeft*: Node
        expressionBinaryRight*: Node

    of NodeKind.FunctionDefinitionArgument:
      functionDefinitionArgumentName*: string
      functionDefinitionArgumentType: Node
      functionDefinitionArgumentDefaultValue*: Node

    of NodeKind.FunctionDefinition:
      functionDefinitionName*: string
      functionDefinitionArguments*: Node
      functionDefinitionReturnType*: Node
      functionDefinitionBody*: Node

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

# proc initType*(identifier: string): Node =
#   if identifier notin BuiltinTypes:
#     result = Node(kind: NodeKind.Type, typeKind: TypeKind.Custom, typeName: identifier)
#   else:
#     case identifier:
#     of "int": result = Node(kind: NodeKind.Type, typeKind: TypeKind.Int)
#     of "float": result = Node(kind: NodeKind.Type, typeKind: TypeKind.Float)
#     of "string": result = Node(kind: NodeKind.Type, typeKind: TypeKind.String)
#     of "bool": result = Node(kind: NodeKind.Type, typeKind: TypeKind.Bool)

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