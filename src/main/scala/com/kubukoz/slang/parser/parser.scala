package com.kubukoz.slang.parser

import com.kubukoz.slang.Failure
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
  val whitespaceChar = Parser.charIn(" \t\r\n")
  val whitespace = whitespaceChar.rep0.void
  def token[A](a: Parser[A]): Parser[A] = a.surroundedBy(whitespace)
  val isKeyword: Name => Boolean = Set("def").compose(_.value)

  def parens[A](expr: Parser[A]): Parser[A] =
    expr.between(char('('), char(')'))

  val alpha: Parser[Char] = Parser.charIn(('a' to 'z') ++ ('A' to 'Z'))

  val number: Parser[Literal.Number] = {
    // Yes, the only literal is 42
    val fortyTwo: Parser[42] = Parser.string("42").as(42)

    fortyTwo.map(Literal.Number(_))
  }

  val literal: Parser[Expr.Literal[Id]] = token(number.map(Expr.Literal(_)))

  val name: Parser[Name] = token {
    def mk(head: Char, tail: List[Char]) = Name(head.toString ++ tail.mkString)

    (alpha ~ alpha.orElse(Numbers.digit).rep0)
      .map(mk.tupled)
      .flatMap {
        case kw if isKeyword(kw) => Parser.failWith(s"Illegal name: ${"\""}${kw.value}${"\""} is a keyword")
        case n => Parser.pure(n)
      }
  }

  val term: Parser[Expr.Term[Id]] = name.map(Expr.Term(_))

  val argument: Parser[Expr.Argument[Id]] = name.map(Expr.Argument(_))

  def functionDef(expr: Parser[Expr[Id]]): Parser[Expr.FunctionDef[Id]] =
    (
      token(string("def") *> whitespaceChar) *> name,
      parens(argument),
      token(char('=')) *> expr
    ).mapN(Expr.FunctionDef[cats.Id])

  val singleExpression: Parser[Expr[Id]] = Parser.recursive { expr =>
    val base = oneOf(
      functionDef(expr).backtrack ::
        term.backtrack ::
        literal ::
        Nil
    )


    // I don't like having this whitespace here...
    // todo: currying
    (base ~ (parens(expr) <* whitespace).backtrack.?).map {
      case (b, None) =>
        b
      case (b, Some(c)) =>
        Expr.Apply(b, c)
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

case class SourceFile(name: String, code: String)

trait SourceParser[F[_]]:
  // Parse a single file to an expression
  def parse(file: SourceFile): F[Expr[Id]]

object SourceParser:
  def apply[F[_]](using SourceParser[F]): SourceParser[F] = summon

  def instance[F[_]: Console](using MonadError[F, Throwable]): SourceParser[F] =
    case SourceFile(fileName, source) =>
      parsing.parser.parseAll(source).leftMap(Failure.Parsing(_)).liftTo[F]

