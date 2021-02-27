package com.kubukoz.slang

import cats.parse.Parser
import com.kubukoz.slang.ast.Name

enum Failure extends Exception:
  case Parsing(failure: Parser.Error)
  case Qualifying(name: Name, context: qualifier.Scope)

  override def toString = this match
    case Parsing(failure) => s"Parsing($failure)"
    case Qualifying(name, context) => s"Qualifying($name, $context)"
