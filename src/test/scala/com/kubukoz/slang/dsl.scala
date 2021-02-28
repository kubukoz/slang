package com.kubukoz.slang
import com.kubukoz.slang.ast._
import cats.Id

object dsl:
  extension(s: String)
    def of(number: 42): Expr.Apply[Id] =
      of(Expr.Literal[Id](Literal.Number(number)))

    def of(argument: String): AppliedName =
      AppliedName(Name(s), Name(argument))

    def of(expr: Expr[Id]): Expr.Apply[Id] =
      Expr.Apply(Expr.Term(Name(s)), expr)

  extension(ap: AppliedName)
    def toExpr: Expr.Apply[Id] = Expr.Apply(Expr.Term(ap.on), Expr.Term(ap.arg))
    def of(anotherArg: String): Expr.Apply[Id] =
      Expr.Apply(toExpr, Expr.Term(Name(anotherArg)))

    def is(body: String): Expr.FunctionDef[Id] = is(Expr.Term[Id](Name(body)))
    def is(body: Expr[Id]): Expr.FunctionDef[Id] = Expr.FunctionDef(
      ap.on,
      Argument(ap.arg),
      body
    )

  case class AppliedName(on: Name, arg: Name)
  object AppliedName:
    given Conversion[AppliedName, Expr.Apply[Id]] = _.toExpr
