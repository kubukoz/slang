package com.kubukoz.slang

import cats.effect.IOApp
import cats.effect.IO
import cats.parse._
import cats._
import cats.implicits._
import java.nio.file.Paths
import fs2.io.file.Files

case class Name(value: String)

enum Expr[F[_]]:
  case Term(name: F[Name])
  case Argument(name: F[Name])
  case FunctionDef(name: F[Name], arg: F[Argument[F]], body: F[Expr[F]])

val parser: Parser[Expr[Id]] = Parser.recursive { expr =>
  import Parser._

  val whitespace = Parser.charsWhile0(_.isWhitespace)

  def token[A](a: Parser[A]): Parser[A] = a.surroundedBy(whitespace)

  val alpha = Parser.charIn(('a' to 'z') ++ ('A' to 'Z'))

  val name = token {
    def mk(head: Char, tail: List[Char]) = Name(head.toString ++ tail.mkString)

    alpha.flatMap { h =>
      alpha.orElse(cats.parse.Numbers.digit)
        .rep0
        .map(mk(h,_))
    }
  }

  val term: Parser[Expr.Term[Id]] = name.map(Expr.Term(_))

  val argument: Parser[Expr.Argument[Id]] = name.map(Expr.Argument(_))

  val functionDef: Parser[Expr.FunctionDef[Id]] =
    token(string("def ")) *> (
      name,
      argument.between(char('('), char(')')),
      token(char('=')) *> expr
    ).mapN(Expr.FunctionDef[cats.Id])

  functionDef <+> term
}


enum Failure extends Exception:
  case Parsing(failure: Parser.Error)

object Main extends IOApp.Simple:

  val run: IO[Unit] =
    Files[IO].readAll(Paths.get("./example.s"), 4096)
      .through(fs2.text.utf8Decode[IO])
      .compile.string
      .flatMap(parser.parseAll(_).leftMap(Failure.Parsing(_)).liftTo[IO])
      .flatMap(IO.println(_))
