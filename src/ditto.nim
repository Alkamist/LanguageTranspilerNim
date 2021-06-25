import std/options

const
  KeyWords* = ["var", "const", "if", "else", "elif", "for", "while", "fn"]
  BuiltinTypes* = ["int", "float", "string", "bool"]
  WhiteSpace* = {' ', '\t', '\v', '\c', '\n', '\f'}
  IdentStartChars* = {'a'..'z', 'A'..'Z', '_'}
  IdentChars* = {'a'..'z', 'A'..'Z', '0'..'9', '_'}
  NumberStartChars* = {'0'..'9', '.'}
  NumberChars* = {'0'..'9', '_', '.'}
  Operators* = ["and", "or", "not", "+", "+=", "-", "-=", "*", "*=", "/", "/=",
                "<", "<=", ">", ">=", "!=", "=", "==", "%", "%=", ".", "(", ")",
                "{", "}", "[", "]", ",", ":", ";"]
  MaxOperatorLength* = block:
    var biggest = 0
    for operator in Operators:
      biggest = max(biggest, operator.len)
    biggest

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
      name*: Node

  VariableDefinitionKind* {.pure.} = enum
    Constant,
    Mutable,

  VariableDefinition* = object
    kind*: VariableDefinitionKind
    name*: Node
    `type`*: Node

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
    And,
    Or,
    Not,
    BangEquals,
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
    Mod,
    ModEquals,

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

  FunctionCall* = object
    name*: Node
    arguments*: Node

  NodeKind* {.pure.} = enum
    None,
    List,
    Identifier,
    Type,
    Literal,
    Expression,
    FunctionCall,
    VariableDefinition,

  Node* = ref NodeObject

  NodeObject* = object
    case kind*: NodeKind
    of NodeKind.None: discard
    of NodeKind.List: list*: List
    of NodeKind.Identifier: identifier*: Identifier
    of NodeKind.Type: `type`*: Type
    of NodeKind.Literal: literal*: Literal
    of NodeKind.Expression: expression*: Expression
    of NodeKind.FunctionCall: functionCall*: FunctionCall
    of NodeKind.VariableDefinition: variableDefinition*: VariableDefinition

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

proc initNone*(): Node =
  Node(kind: NodeKind.None)

proc initIntLiteral*(value: int): Node =
  Node(
    kind: NodeKind.Literal,
    literal: Literal(
      kind: LiteralKind.Int,
      intValue: value,
    ),
  )

proc initFloatLiteral*(value: float): Node =
  Node(
    kind: NodeKind.Literal,
    literal: Literal(
      kind: LiteralKind.Float,
      floatValue: value,
    ),
  )

proc initBoolLiteral*(value: bool): Node =
  Node(
    kind: NodeKind.Literal,
    literal: Literal(
      kind: LiteralKind.Bool,
      boolValue: value,
    ),
  )

proc initStringLiteral*(value: string): Node =
  Node(
    kind: NodeKind.Literal,
    literal: Literal(
      kind: LiteralKind.String,
      stringValue: value,
    ),
  )

proc initIdentifier*(name: string): Node =
  Node(
    kind: NodeKind.Identifier,
    identifier: Identifier(name: name),
  )

proc initType*(kind: TypeKind, name = none(Node)): Node =
  if kind == TypeKind.Custom:
    return Node(kind: NodeKind.Type, `type`: Type(kind: kind, name: name))
  else:
    return Node(kind: NodeKind.Type, `type`: Type(kind: kind))

proc initVariableDefinition*(kind: VariableDefinitionKind,
                             name: Node,
                             `type` = initNone()): Node =
  Node(
    kind: NodeKind.VariableDefinition,
    variableDefinition: VariableDefinition(
      kind: kind,
      name: name,
      `type`: `type`,
    ),
  )

proc initUnaryExpression*(kind: UnaryExpressionKind, value = initNone()): Node =
  Node(
    kind: NodeKind.Expression,
    expression: Expression(
      kind: ExpressionKind.Unary,
      unary: UnaryExpression(kind: kind, value: value),
    ),
  )

proc initBinaryExpression*(kind: BinaryExpressionKind;
                          left, right = initNone()): Node =
  Node(
    kind: NodeKind.Expression,
    expression: Expression(
      kind: ExpressionKind.Binary,
      binary: BinaryExpression(kind: kind, left: left, right: right),
    ),
  )

proc initFunctionCall*(name: Node, arguments = initNone()): Node =
  Node(
    kind: NodeKind.FunctionCall,
    functionCall: FunctionCall(
      name: name,
      arguments: arguments,
    ),
  )

proc initStatementList*(): Node =
  Node(
    kind: NodeKind.List,
    list: List(kind: ListKind.Statement),
  )

proc initCommaList*(): Node =
  Node(
    kind: NodeKind.List,
    list: List(kind: ListKind.Comma),
  )

proc toTypeKind*(text: String): TypeKind =
  case text:
  of "int": TypeKind.Int
  of "float": TypeKind.Float
  of "bool": TypeKind.Bool
  of "string": TypeKind.String
  else: TypeKind.Custom

proc toType*(identifier: Identifier): Type =
  let typeKind = identifier.name.toTypeKind
  if typeKind == TypeKind.Custom:
    return initType(typeKind, identifier)
  else:
    return initType(typeKind)

proc toBinaryExpressionKind*(text: string): Option[BinaryExpressionKind] =
  case text:
  of "and": some(BinaryExpressionKind.And)
  of "or": some(BinaryExpressionKind.Or)
  of "not": some(BinaryExpressionKind.Not)
  of "!=": some(BinaryExpressionKind.BangEquals)
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
  of "%": some(BinaryExpressionKind.Mod)
  of "%=": some(BinaryExpressionKind.ModEquals)
  else: none(BinaryExpressionKind)

proc toVariableDefinitionKind*(text: string): Option[VariableDefinitionKind] =
  case text:
  of "const": some(VariableDefinitionKind.Constant)
  of "var": some(VariableDefinitionKind.Mutable)
  else: none(VariableDefinitionKind)

proc toDitto*(s: Node): string

proc toDitto*(s: Identifier): string =
  s.name

proc toDitto*(s: VariableDefinitionKind): string =
  case s:
  of VariableDefinitionKind.Constant: "const"
  of VariableDefinitionKind.Mutable: "var"

proc toDitto*(s: VariableDefinition): string =
  result.add(s.kind.toDitto)
  result.add(" ")
  result.add(s.name.toDitto)
  if s.`type`.kind != NodeKind.None:
    result.add(" ")
    result.add(s.`type`.toDitto)

proc toDitto*(s: Literal): string =
  case s.kind:
  of LiteralKind.Int: $s.intValue
  of LiteralKind.Float: $s.floatValue
  of LiteralKind.String: "\"" & $s.stringValue & "\""
  of LiteralKind.Bool: $s.boolValue

proc toDitto*(s: Type): string =
  case s.kind:
  of TypeKind.Int: "int"
  of TypeKind.Float: "float"
  of TypeKind.String: "string"
  of TypeKind.Bool: "bool"
  of TypeKind.Custom: s.name.toDitto

proc toDitto*(s: List): string =
  case s.kind:

  of ListKind.Statement:
    for i, child in s.children:
      result.add(child.toDitto)
      result.add(";")
      if i + 1 < s.children.len:
        result.add("\n")

  of ListKind.Comma:
    for i, child in s.children:
      result.add(child.toDitto)
      if i + 1 < s.children.len:
        result.add(", ")

proc toDitto*(s: BinaryExpressionKind): string =
  case s:
  of BinaryExpressionKind.And: "and"
  of BinaryExpressionKind.Or: "or"
  of BinaryExpressionKind.Not: "not"
  of BinaryExpressionKind.BangEquals: "!="
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
  of BinaryExpressionKind.Mod: "%"
  of BinaryExpressionKind.ModEquals: "%="

proc toDitto*(s: Expression): string =
  case s.kind:

  of ExpressionKind.Unary:
    if s.unary.kind == UnaryExpressionKind.Minus:
      result.add("-")
    result.add(s.unary.value.toDitto)

  of ExpressionKind.Binary:
    # result.add(s.binary.kind.toDitto)
    # result.add("(")
    # result.add(s.binary.left.toDitto)
    # result.add(", ")
    # result.add(s.binary.right.toDitto)
    # result.add(")")

    result.add(s.binary.left.toDitto)
    result.add(" ")
    result.add(s.binary.kind.toDitto)
    result.add(" ")
    result.add(s.binary.right.toDitto)

proc toDitto*(s: FunctionCall): string =
  result.add(s.name.toDitto)
  result.add("(")
  result.add(s.arguments.toDitto)
  result.add(")")

proc toDitto*(s: Node): string =
  case s.kind:
  of NodeKind.None: "NONE"
  of NodeKind.List: s.list.toDitto
  of NodeKind.Identifier: s.identifier.toDitto
  of NodeKind.Type: s.`type`.toDitto
  of NodeKind.Literal: s.literal.toDitto
  of NodeKind.Expression: s.expression.toDitto
  of NodeKind.FunctionCall: s.functionCall.toDitto
  of NodeKind.VariableDefinition: s.variableDefinition.toDitto