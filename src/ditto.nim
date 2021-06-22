type
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

  NodeKind* {.pure.} = enum
    Type,
    Literal,
    FunctionArgument,
    FunctionDefinition,

  Node* = ref NodeObject

  NodeObject* = object
    case kind*: NodeKind

    of NodeKind.Type:
      case typeKind*: TypeKind
      of TypeKind.Int, TypeKind.Float, TypeKind.String, TypeKind.Bool: discard
      of TypeKind.Custom: typeKindCustomValue*: string

    of NodeKind.Literal:
      case literalKind*: LiteralKind
      of LiteralKind.Int: literalIntValue*: int
      of LiteralKind.Float: literalFloatValue*: float
      of LiteralKind.String: literalStringValue*: string
      of LiteralKind.Bool: literalBoolValue*: bool

    of NodeKind.FunctionArgument:
      functionArgumentName*: string
      functionArgumentType*: Node
      functionArgumentValue*: Node

    of NodeKind.FunctionDefinition:
      functionDefinitionName*: string
      functionDefinitionArguments*: Node
      functionDefinitionReturnType*: Node
      functionDefinitionBody*: Node

const
  KeyWords* = ["var", "const", "if", "else", "elif", "for", "while", "fn"]
  BuiltinTypes* = ["i8", "i16", "i32", "i64",
                  "u8", "u16", "u32", "u64",
                  "f32", "f64", "size",
                  "bool", "string"]
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