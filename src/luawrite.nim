proc writeLuaFunctionDefinition(isLocal: bool,
                                name: string,
                                arguments: seq[string],
                                body: string): string =
  if isLocal:
    result.add("local ")

  result.add("function ")
  result.add(name)
  result.add("(")

  for i, argument in arguments:
    result.add(argument)

    if i < arguments.len - 1:
      result.add(", ")

  result.add(")\n")
  result.add(body)
  result.add("\n")
  result.add("end\n")