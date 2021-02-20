package com.kubukoz.slang

import cats.effect.IOApp
import cats.effect.IO
import cats.parse._
import cats._
import cats.implicits._
import java.nio.file.Paths
import fs2.io.file.Files
import cats.data.StateT

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

case class Scope(functions: List[Expr.FunctionDef[Id]]):
  import monocle.syntax.all._
  import monocle.Focus.focus

  def addFunction(function: Expr.FunctionDef[Id]): Scope = this.focus(_.functions).modify(function :: _)

object Scope:
  val init: Scope = Scope(Nil)

trait Scoped[F[_], S]:
  def scope[A](f: S => S)(fa: F[A]): F[A]

object Scoped:
  def apply[F[_], S](using Scoped[F, S]): Scoped[F, S] = summon
  type Of[S] = [F[_]] =>> Scoped[F, S]

  given [F[_]: Monad, S]: Scoped[StateT[F, S, *], S] = new Scoped[StateT[F, S, *], S]:
    def scope[A](f: S => S)(fa: StateT[F, S, A]): StateT[F, S, A] = StateT { state =>
      val localState = f(state)

      fa.run(localState)
    }

trait Interpreter[F[_]]:
  def run(program: Expr[Id]): F[Unit]

object Interpreter:
  def apply[F[_]](using Interpreter[F]): Interpreter[F] = summon
  def instance[F[_]: Scoped.Of[Scope]: Monad]: Interpreter[F] = new Interpreter[F]:
    def run(program: Expr[Id]): F[Unit] = program match {
      case f: Expr.FunctionDef[Id] => Scoped[F, Scope].scope(_.addFunction(f))(Applicative[F].unit)
      case _ => Applicative[F].unit
    }

enum Failure extends Exception:
  case Parsing(failure: Parser.Error)

object Main extends IOApp.Simple:

  type InterpreterState[A] = StateT[IO, Scope, A]

  given Interpreter[InterpreterState] = Interpreter.instance

  val run: IO[Unit] =
    Files[IO].readAll(Paths.get("./example.s"), 4096)
      .through(fs2.text.utf8Decode[IO])
      .compile.string
      .flatMap(parser.parseAll(_).leftMap(Failure.Parsing(_)).liftTo[IO])
      .flatTap(IO.println(_))
      .flatMap { expr =>
         Interpreter[StateT[IO, Scope, *]].run(expr).run(Scope.init)
      }
      .flatMap(IO.println(_))
