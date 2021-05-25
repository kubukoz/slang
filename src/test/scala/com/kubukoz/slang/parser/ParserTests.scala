package com.kubukoz.slang.parser

import weaver.*
import cats.implicits.*
import com.kubukoz.slang.*
import com.kubukoz.slang.ast.Literal as LLiteral
import com.kubukoz.slang.ast.Expr
import com.kubukoz.slang.ast.Expr.*
import com.kubukoz.slang.ast.Literal.*
import cats.Id
import cats.effect.IO
import dsl.*
import fs2.io.file.*
import java.nio.file.Paths

object ParserTests extends SimpleIOSuite {
  import parsing.parser.parseAll as parse

  // might be unused, but keep it there as a utility
  def parsePretty(text: String): Either[String, Expr[Id]] =
    parse(text).leftMap(prettyPrint("test-data", text, _))

  def simpleParserTest(text: String)(result: Expr[Id])(implicit loc: SourceLocation) =
    pureTest(text) {
      assert(parse(text) == Right(result))
    }

  simpleParserTest("42")(Literal[Id](Number(42)))

  simpleParserTest("hello(world)") {
    "hello".of("world")
  }

  simpleParserTest("hello ( world )") {
    "hello".of("world")
  }

  pureTest("hello ( 42 ) ") {
    // this can't be inlined apparently
    // possible bug in weaver wrt overloads?
    val expected = "hello".of(42)
    assert {
      parse("hello ( 42 ) ") == Right(expected)
    }
  }

  pureTest("currying: hello(foo)(bar)") {
    val expected = "hello".of("foo").of("bar")
    assert {
      parse("hello(foo)(bar)") == Right(expected)
    }
  }

  // //local definition expression (bar is local to foo)
  simpleParserTest("def foo(arg) = def bar(a) = arg(a)") {
    "foo"
      .of("arg")
      .is(
        "bar".of("a").is("arg".of("a"))
      )
  }

  simpleParserTest("def demo( fun ) = fun") {
    "demo".of("fun").is("fun")
  }

  locally {
    val selfRecIdentity =
      "identity".of("arg").is("identity".of("arg"))

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
    "soMuchFun0"
      .of("arg")
      .is(
        "foo".of("a").is("a")
      )
  }

  pureTest("two lines") {
    val result = block[Id](
      "identity".of("arg").is("arg"),
      "soMuchFun".of("arg").is("identity".of("arg"))
    )

    assert(
      parse("""def identity(arg) = arg
              |def soMuchFun(arg) = identity(arg)""".stripMargin) == Right(result)
    )
  }

  test("can parse example") {

    val srcIO = Files[IO].readAll(Paths.get("./example.sp"), 4096).through(fs2.text.utf8Decode[IO]).compile.string

    val expected = block[Id](
      "identity".of("arg").is("arg"),
      "soMuchFun0".of("arg").is("foo".of("a").is("a")),
      "soMuchFun".of("arg").is("identity".of("arg")),
      "evenMoreFun".of("arg").is("soMuchFun".of("soMuchFun".of("arg"))),
      "println".of("addOne".of("addOne".of("addOne".of("addOne".of(42))))),
      "println".of("currentTime")
    )

    srcIO.map { src =>
      assert {
        parse(src) == Right(expected)
      }
    }
  }
}
