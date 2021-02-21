package com.kubukoz.slang

import cats.parse.Parser

enum Failure extends Exception:
  case Parsing(failure: Parser.Error)
