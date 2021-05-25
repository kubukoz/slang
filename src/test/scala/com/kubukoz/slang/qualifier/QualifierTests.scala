package com.kubukoz.slang.qualifier

import weaver.*
import cats.implicits.*
import com.kubukoz.slang.*
import com.kubukoz.slang.ast.*
import com.kubukoz.slang.ast.Literal.*
import cats.Id
import cats.data.Chain
import cats.effect.IO
import scala.util.chaining.*
import dsl.*

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
      "identity".of("arg").is("arg")
    )(
      "identity".of("identity(arg)").is("identity(arg)")
    )
  }

  test("nested function") {
    simpleQualifierTest(
      "f1".of("arg").is("f2".of("arg2").is("arg2"))
    )(
      "f1".of("f1(arg)").is("f1.f2".of("f1.f2(arg2)").is("f1.f2(arg2)"))
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
      "println".of(42)
    )(
      "<builtins>.println".of(42)
    )
  }

  test("scope doesn't leak from function") {
    qualify[IO]
      .andThen(requireFailure) {
        Expr.block(
          "identity".of("arg").is("arg"),
          "identity2".of("arg2").is("arg")
        )
      }
      .map { actual =>
        val rootNames = Map(Name("identity") -> Name("identity"), Name("identity2") -> Name("identity2"))
        val scope =
          Scope
            .root
            .fork // block scope
            .addNames(rootNames)
            .fork // function scope
            .addNames(Map(Name("arg2") -> Name("identity2(arg2)")))
            .addPath("identity2")

        assert(actual == Failure.Qualifying(Name("arg"), scope))
      }
  }

  test("simple self-recursion") {
    simpleQualifierTest(
      "never".of("a").is("never".of("a"))
    )(
      "never".of("never(a)").is("never".of("never(a)"))
    )
  }

  test("fixpoint combinator: fix(f) = f(fix(f))") {
    simpleQualifierTest(
      "fix".of("f").is("f".of("fix".of("f")))
    )(
      "fix".of("fix(f)").is("fix(f)".of("fix".of("fix(f)")))
    )
  }

  test("infinite mutual recursion") {
    val input = Expr.block(
      "f1".of("a").is("f2".of("a")),
      "f2".of("a").is("f1".of("a"))
    )

    simpleQualifierTest(input) {
      Expr.block(
        "f1".of("f1(a)").is("f2".of("f1(a)")),
        "f2".of("f2(a)").is("f1".of("f2(a)"))
      )
    }
  }
}
