package com.kubukoz.slang.qualifier

import weaver._
import cats.implicits._
import com.kubukoz.slang._
import com.kubukoz.slang.ast._
import com.kubukoz.slang.ast.Literal._
import cats.Id
import cats.data.Chain
import cats.effect.IO
import scala.util.chaining._

object QualifierTests extends SimpleIOSuite {
  type Result[A] = Either[Throwable, A]

  def simpleQualifierTest(expr: Expr[Id])(expected: Expr[Id]) =
    qualify[IO](expr).map { actual =>
      assert(actual == expected)
    }

  def requireFailure(io: IO[Any]): IO[Throwable] = io
    .attempt
    .map {
      _.swap.leftMap(result => new Throwable("Expected failure: " + result))
    }
    .rethrow

  test("println") {
    simpleQualifierTest(
      Expr.Term(Name("println"))
    )(
      Expr.Term(Name("<builtins>.println"))
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
        Argument(Name("arg")),
        Expr.Term(Name("arg"))
      )
    )(
      Expr.FunctionDef(
        Name("identity"),
        Argument(Name("identity(arg)")),
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

  test("scope doesn't leak from function") {
    qualify[IO]
      .andThen(requireFailure) {
        Expr.block(
          Expr.FunctionDef(
            Name("identity"),
            Argument(Name("arg")),
            Expr.Term(Name("arg"))
          ),
          Expr.FunctionDef(
            Name("identity2"),
            Argument(Name("arg2")),
            Expr.Term(Name("arg"))
          )
        )
      }
      .map { actual =>
        val scope =
          Scope(
            currentPath = Chain("identity2"),
            currentLocalNames = Map(Name("arg2") -> Name("identity2(arg2)"))
          )

        assert(actual == Failure.Qualifying(Name("arg"), scope))
      }
  }

  test("simple self-recursion") {
    val input = Expr.FunctionDef[Id](
      Name("never"),
      Argument(Name("a")),
      Expr.Apply(
        Expr.Term(Name("never")),
        Expr.Term(Name("a"))
      )
    )

    simpleQualifierTest(input) {
      Expr.FunctionDef[Id](
        Name("never"),
        Argument(Name("never(a)")),
        Expr.Apply(
          Expr.Term[Id](Name("never")),
          Expr.Term(Name("never(a)"))
        )
      )
    }
  }
}
