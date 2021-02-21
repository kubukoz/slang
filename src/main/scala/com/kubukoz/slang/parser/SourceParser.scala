package com.kubukoz.slang.parser

import cats.effect.std.Console
import cats.parse.Parser
import cats.parse.Numbers
import cats.parse.LocationMap
import cats.parse.Parser.Expectation
import cats.data.NonEmptyList
import cats.Id
import cats.MonadError
import cats.syntax.all._
import com.kubukoz.slang.ast._

import Parser._

object parsing:
  val whitespace = Parser.charIn(" \t\r\n").rep0.void
  def token[A](a: Parser[A]): Parser[A] = a.surroundedBy(whitespace)
  val isKeyword: Name => Boolean = Set("def").compose(_.value)

  def parens[A](expr: Parser[A]): Parser[A] =
    expr.between(char('('), char(')'))

  val alpha: Parser[Char] = Parser.charIn(('a' to 'z') ++ ('A' to 'Z'))

  // Yes, the only literal is 42
  val number: Parser[Expr.Literal[Id]] = {
    val fortyTwo: Parser[42] = Parser.string("42").as(42)

    fortyTwo.map(Literal.Number(_)).map(Expr.Literal(_))
  }

  val literal: Parser[Expr.Literal[Id]] = token(number)

  val name: Parser[Name] = token {
    def mk(head: Char, tail: List[Char]) = Name(head.toString ++ tail.mkString)

    alpha.flatMap { h =>
      alpha.orElse(Numbers.digit)
        .rep0
        .map(mk(h,_))
    }.flatMap {
      case kw if isKeyword(kw) => Parser.failWith(s"Illegal name: ${"\""}${kw.value}${"\""} is a keyword")
      case n => Parser.pure(n)
    }
  }

  val term: Parser[Expr.Term[Id]] = name.map(Expr.Term(_))

  val argument: Parser[Expr.Argument[Id]] = name.map(Expr.Argument(_))

  def functionDef(expr: Parser[Expr[Id]]): Parser[Expr.FunctionDef[Id]] =
    (
      token(string("def ")) *> name,
      parens(argument),
      token(char('=')) *> expr
    ).mapN(Expr.FunctionDef[cats.Id])

  val singleExpression: Parser[Expr[Id]] = Parser.recursive { expr =>
    val base = oneOf(functionDef(expr) :: term :: literal :: Nil)
    val maybeApplication = parens(expr) <* whitespace

    base.flatMap { b =>
      maybeApplication.map(Expr.Apply(b, _)).orElse(Parser.pure(b))
    }
  }

  val parser: Parser[Expr[Id]] =
    singleExpression.rep.map {
      case NonEmptyList(one, Nil) => one
      case more =>
        // implicit block
        Expr.Block(more)
    }

end parsing

enum Failure extends Exception:
  case Parsing(failure: Parser.Error)

case class SourceFile(name: String, code: String)

trait SourceParser[F[_]]:
  // Parse a single file to an expression
  def parse(file: SourceFile): F[Expr[Id]]

object SourceParser:
  def apply[F[_]](using SourceParser[F]): SourceParser[F] = summon

  def instance[F[_]: Console](using MonadError[F, Throwable]): SourceParser[F] =
    case SourceFile(fileName, source) =>
      parsing.parser.parseAll(source) match
        case Left(e) => Console[F].errorln(prettyPrint(fileName, source, e)) *> Failure.Parsing(e).raiseError
        case Right(v) => v.pure[F]
