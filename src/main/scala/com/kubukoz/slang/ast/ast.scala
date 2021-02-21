package com.kubukoz.slang.ast

import cats.data.NonEmptyList

case class Name(value: String)

enum Expr[F[_]]:
  case Term(name: F[Name])
  case TermApply(on: Term[F], param: Expr[F])
  case Argument(name: F[Name])
  case Block(expressions: NonEmptyList[Expr[F]])
  case FunctionDef(name: F[Name], arg: F[Argument[F]], body: F[Expr[F]])
