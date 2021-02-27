package com.kubukoz.slang

import cats.Id
import cats.Monad
import cats.Applicative
import cats.effect.Clock
import cats.effect.std.Console
import cats.data.StateT
import com.kubukoz.slang.ast._
import cats.implicits._

trait Interpreter[F[_]]:
  def run(program: Expr[Id]): F[Unit]

object Interpreter:
  def apply[F[_]](using Interpreter[F]): Interpreter[F] = summon
  def instance[F[_]: Scoped.Of[Scope]: Monad: Console: Clock]: Interpreter[F] = new Interpreter[F]:
    def run(program: Expr[Id]): F[Unit] = program match
      case f: Expr.FunctionDef[Id] => Scoped[F, Scope].scope(_.addFunction(f))(Applicative[F].unit)
      case Expr.Apply(function, param) =>
        function match
          case Expr.Term(Name("println")) =>
            def evalParam(e: Expr[Id]): F[Any] = e match
              case Expr.Literal(Literal.Number(num)) => num.pure[F]
              case Expr.Apply(Expr.Term(Name("addOne")), e) => evalParam(e).map(_.asInstanceOf[Int] + 1)
              case Expr.Term(Name("currentTime")) => Clock[F].realTimeInstant.widen
              case e => throw new Exception("I can't do this yet: " + e)

            evalParam(param).flatMap(Console[F].println(_))
          case e => Applicative[F].unit
      case Expr.Block(expressions) => expressions.traverse_(run)
      case _ => Applicative[F].unit

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
