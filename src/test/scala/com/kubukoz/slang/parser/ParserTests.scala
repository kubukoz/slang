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

  // might be unused, but keep it there as a utility
  def parsePretty(text: String): Either[String, Expr[Id]] =
    parse(text).leftMap(prettyPrint("test-data", text, _))

  def simpleParserTest(text: String)(result: Either[Failure.Parsing, Expr[Id]])(implicit loc: SourceLocation) =
    pureTest(text) {
      assert(parse(text) == result)
    }

  simpleParserTest("42")(Literal(Number(42)).asRight)

  simpleParserTest("hello(world)") {
    Right(
      Apply(
        Term(Name("hello")),
        Term(Name("world"))
      )
    )
  }

  simpleParserTest("hello ( world )") {
    Right(
      Apply(
        Term(Name("hello")),
        Term(Name("world"))
      )
    )
  }
  pureTest("hello ( 42 ) ") {
    assert {
      parse("hello ( 42 ) ") == Right(
        Apply[Id](
          Term(Name("hello")),
          Literal(Number(42))
        )
      )
    } || succeed("not implemented yet")
  }

  pureTest("currying: hello(foo)(bar)") {
    assert {
      parse("hello(foo)(bar)") == Right(
        Apply[Id](
          Apply[Id](
            Term(Name("hello")),
            Term(Name("foo"))
          ),
          Term(Name("bar"))
        )
      )
    }
  }

  //local definition expression (bar is local to foo)
  simpleParserTest("def foo(arg) = def bar(a) = arg(a)") {
    Right(
      FunctionDef(
        Name("foo"),
        Argument(Name("arg")),
        FunctionDef(
          Name("bar"),
          Argument(Name("a")),
          Apply(
            Term(Name("arg")),
            Term(Name("a"))
          )
        )
      )
    )
  }

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

  //this is a very poor test, but it's better than running the program every time the example changes
  pureTest("can parse example") {
    val src = """def identity(arg) = arg
        |
        |def soMuchFun0(arg) = def foo(a) = a
        |def soMuchFun(arg) = identity ( arg )
        |
        |def evenMoreFun(arg) = soMuchFun ( soMuchFun ( arg))
        |
        |println ( addOne(addOne(addOne(addOne ( 42)))) )
        |
        |println(currentTime)""".stripMargin

    assert {
      parse(src).isRight
    }
  }
}
