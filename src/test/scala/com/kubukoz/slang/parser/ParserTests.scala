package com.kubukoz.slang.parser

import weaver._
import cats.implicits._
import com.kubukoz.slang._
import com.kubukoz.slang.ast.{Literal => LLiteral, _}
import com.kubukoz.slang.ast.Expr._
import com.kubukoz.slang.ast.Literal._
import cats.Id

object ParserTests extends SimpleIOSuite {
  import parsing.parser.{parseAll => parse}

  // SourceParser
  def simpleParserTest(text: String)(result: Either[Failure.Parsing, Expr[Id]])(implicit loc: SourceLocation) =
    pureTest(text) {
      assert(parse(text) == result)
    }

  simpleParserTest("42")(Literal(Number(42)).asRight)

  simpleParserTest("def demo( fun ) = fun") {
    Right(
      FunctionDef(
        Name("demo"),
        Argument(Name("fun")),
        Term(Name("fun"))
      )
    )
  }

  locally {
    val selfRecIdentity = Right(
      FunctionDef[Id](
        Name("identity"),
        Argument(Name("arg")),
        Apply(
          Term(Name("identity")),
          Term(Name("arg"))
        )
      )
    )

    simpleParserTest("def identity(arg) = identity(arg)") {
      selfRecIdentity
    }

    simpleParserTest("def identity (arg) = identity(arg)") {
      selfRecIdentity
    }
    simpleParserTest("def identity ( arg) = identity(arg)") {
      selfRecIdentity
    }

    simpleParserTest("def identity ( arg ) = identity ( arg  )") {
      selfRecIdentity
    }
  }

  simpleParserTest("def soMuchFun0(arg) = def foo(a) = a") {
    Right(
      FunctionDef(
        Name("soMuchFun0"),
        Argument(Name("arg")),
        FunctionDef(
          Name("foo"),
          Argument(Name("a")),
          Term(Name("a"))
        )
      )
    )
  }

  pureTest("two lines") {
    val result = block[Id](
      FunctionDef(
        Name("identity"),
        Argument(Name("arg")),
        Term(Name("arg"))
      ),
      FunctionDef(
        Name("soMuchFun"),
        Argument(Name("arg")),
        Apply(
          Term(Name("identity")),
          Term(Name("arg"))
        )
      )
    )

    assert(
      parse("""def identity(arg) = arg
              |def soMuchFun(arg) = identity(arg)""".stripMargin) == Right(result)
    )
  }
}
