package com.kubukoz.slang

import cats.parse.Parser
import com.kubukoz.slang.ast.Name

enum Failure extends Exception:
  case Parsing(failure: Parser.Error)
  case Qualifying(name: Name, context: qualifier.Scope)
