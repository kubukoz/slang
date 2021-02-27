package com.kubukoz.slang.qualifier

import weaver._
import cats.implicits._
import com.kubukoz.slang._
import com.kubukoz.slang.ast._
import com.kubukoz.slang.ast.Literal._
import cats.Id
import cats.effect.IO

object QualifierTests extends SimpleIOSuite {
  type Result[A] = Either[Throwable, A]

  def simpleQualifierTest(expr: Expr[Id])(expected: Expr[Id]) =
    qualify[IO](expr).map { actual =>
      assert(actual == expected)
    }

  test("println") {
    simpleQualifierTest(
      Expr.Term(Name("println"))
    )(
      Expr.Term[Id](Name("<builtins>.println"))
    )
  }

  test("literal") {
    simpleQualifierTest(
      Expr.Literal(Literal.Number(42))
    )(
      Expr.Literal(Literal.Number(42))
    )
  }

  test("function argument") {
    simpleQualifierTest(
      Expr.FunctionDef(
        Name("identity"),
        Expr.Argument(Name("arg")),
        Expr.Term(Name("arg"))
      )
    )(
      Expr.FunctionDef(
        Name("identity"),
        Expr.Argument(Name("identity(arg)")),
        // todo: name as enum, in this case it would be a local function parameter
        // so syntax is like function parameters, but whaddaya know
        Expr.Term(Name("identity(arg)"))
      )
    )
  }

  //todo: prop test candidate
  test("single elem block") {
    simpleQualifierTest(
      Expr.block(
        Expr.Term(Name("println"))
      )
    )(
      Expr.block(
        Expr.Term(Name("<builtins>.println"))
      )
    )
  }

  test("simple function apply") {
    simpleQualifierTest(
      Expr.Apply(
        Expr.Term(Name("println")),
        Expr.Literal(Literal.Number(42))
      )
    )(
      Expr.Apply(
        Expr.Term(Name("<builtins>.println")),
        Expr.Literal(Literal.Number(42))
      )
    )
  }
}
