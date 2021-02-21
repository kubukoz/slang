package com.kubukoz.slang

import cats.effect.std.Console
import cats.parse.Parser
import cats.parse.LocationMap
import cats.parse.Parser.Expectation
import cats.data.NonEmptyList
import cats.Id
import cats.MonadError
import cats.syntax.all._

case class Name(value: String)

enum Expr[F[_]]:
  case Term(name: F[Name])
  case TermApply(on: Term[F], param: Expr[F])
  case Argument(name: F[Name])
  case Block(expressions: NonEmptyList[Expr[F]])
  case FunctionDef(name: F[Name], arg: F[Argument[F]], body: F[Expr[F]])

import Parser._

object parsing:
  val whitespace = Parser.charIn(" \t\r\n").rep0.void
  def token[A](a: Parser[A]): Parser[A] = a.surroundedBy(whitespace)
  val isKeyword: Name => Boolean = Set("def").compose(_.value)

import parsing._

val singleExpression: Parser[Expr[Id]] = Parser.recursive { expr =>

  val alpha = Parser.charIn(('a' to 'z') ++ ('A' to 'Z'))

  val name = token {
    def mk(head: Char, tail: List[Char]) = Name(head.toString ++ tail.mkString)

    alpha.flatMap { h =>
      alpha.orElse(cats.parse.Numbers.digit)
        .rep0
        .map(mk(h,_))
    }.flatMap {
      case kw if isKeyword(kw) => Parser.failWith(s"Illegal name: ${"\""}${kw.value}${"\""} is a keyword")
      case n => Parser.pure(n)
    }
  }

  val term: Parser[Expr.Term[Id]] = name.map(Expr.Term(_))

  val argument: Parser[Expr.Argument[Id]] = name.map(Expr.Argument(_))

  val termApply: Parser[Expr.TermApply[Id]] =
    (term, argument.between(char('('), char(')'))).mapN(Expr.TermApply.apply)

  val functionDef: Parser[Expr.FunctionDef[Id]] =
    token(string("def ")) *> (
      name,
      argument.between(char('('), char(')')),
      token(char('=')) *> expr
    ).mapN(Expr.FunctionDef[cats.Id])

  oneOf(functionDef :: termApply :: term :: Nil)
}

val parser: Parser[Expr[Id]] =
  singleExpression.rep.map {
    case NonEmptyList(one, Nil) => one
    case more =>
      // implicit block
      Expr.Block(more)
  }


enum Failure extends Exception:
  case Parsing(failure: Parser.Error)

trait SourceParser[F[_]]:
  // Parse a single file to an expression
  def parse(fileName: String, source: String): F[Expr[Id]]

object SourceParser:
  def apply[F[_]](using SourceParser[F]): SourceParser[F] = summon

  def prettyPrint(fileName: String, source: String, e: Parser.Error): String =
    def colorize(color: String)(text: String) = text.linesWithSeparators.map(color ++ _).mkString

    val (codeBefore, codeAfter) = source.splitAt(e.failedAtOffset).bimap(colorize(scala.Console.GREEN), colorize(scala.Console.RED))

    def expected(s: String) = s"Expected $s"
    val explainExpectation: Expectation => String =
      case Expectation.FailWith(_, msg) => msg
      case Expectation.InRange(_, from, to) if from == to => expected(from.toString)
      // does this even make sense?
      case Expectation.InRange(_, from, to) => expected(s"one of ($from..$to)")
      case Expectation.EndOfString(_, _) => expected(s"end of file")
      case e => e.toString ++ " (unsupported failure)"

    val failureString = e.expected match
      case NonEmptyList(one, Nil) => explainExpectation(one)
      case more => "one of: " ++ more.map(explainExpectation).mkString_(", ")

      val lm = LocationMap(source)
      val location = lm.toLineCol(e.failedAtOffset).fold(s"offset ${e.failedAtOffset}") {
        case (line, column) => s"""line $line, column $column""".stripMargin
      }

    val totalLines = source.linesIterator.size
    val lineCountLength = Math.log10(totalLines).toInt + 1
    val allCode = s"""$codeBefore😡 $codeAfter""".linesWithSeparators.zipWithIndex.map {
      case (line, index) => s"""%${lineCountLength}d | $line""".format(index + 1)
    }.mkString

    s"""${"😭" * 10}
       |Unexpected parsing error in file ${scala.Console.MAGENTA}$fileName${scala.Console.RESET} at $location:
       |${scala.Console.RED}$failureString${scala.Console.RESET}
       |
       |Context:
       |$allCode""".stripMargin
  end prettyPrint

  def instance[F[_]: Console](using MonadError[F, Throwable]): SourceParser[F] = (fileName, source) =>
    parser.parseAll(source) match
      case Left(e) => Console[F].errorln(prettyPrint(fileName, source, e)) *> Failure.Parsing(e).raiseError
      case Right(v) => v.pure[F]
