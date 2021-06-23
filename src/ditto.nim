import std/options

type
  TypeKind* {.pure.} = enum
    Int,
    Float,
    String,
    Bool,
    Custom,

  Type* = object
    case kind*: TypeKind
    of TypeKind.Int, TypeKind.Float, TypeKind.String, TypeKind.Bool: discard
    of TypeKind.Custom: customValue*: string

  LiteralKind* {.pure.} = enum
    Int,
    Float,
    String,
    Bool,

  Literal* = object
    case kind*: LiteralKind
    of LiteralKind.Int: intValue*: int
    of LiteralKind.Float: floatValue*: float
    of LiteralKind.String: stringValue*: string
    of LiteralKind.Bool: boolValue*: bool

  FunctionDefinitionArgument* = object
    name*: string
    `type`*: Option[Type]
    defaultValue*: Option[Node]

  FunctionDefinition* = object
    name*: string
    arguments*: seq[FunctionDefinitionArgument]
    returnType*: Option[Type]
    body*: seq[Node]

  NodeKind* {.pure.} = enum
    Type,
    Literal,
    FunctionDefinition,

  Node* = ref NodeObject

  NodeObject* = object
    case kind*: NodeKind
    of NodeKind.Type: typeValue*: Type
    of NodeKind.Literal: literalValue*: Literal
    of NodeKind.FunctionDefinition: functionDefinitionValue: FunctionDefinition

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

proc initType*(identifier: string): Type =
  if identifier notin BuiltinTypes:
    result = Type(kind: TypeKind.Custom, customValue: identifier)
  else:
    case identifier:
    of "int": result = Type(kind: TypeKind.Int)
    of "float": result = Type(kind: TypeKind.Float)
    of "string": result = Type(kind: TypeKind.String)
    of "bool": result = Type(kind: TypeKind.Bool)