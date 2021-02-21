package com.kubukoz.slang

import cats.Id
import cats.Monad
import cats.Applicative
import cats.data.StateT
import com.kubukoz.slang.ast._

trait Interpreter[F[_]]:
  def run(program: Expr[Id]): F[Unit]

object Interpreter:
  def apply[F[_]](using Interpreter[F]): Interpreter[F] = summon
  def instance[F[_]: Scoped.Of[Scope]: Monad]: Interpreter[F] = new Interpreter[F]:
    def run(program: Expr[Id]): F[Unit] = program match
      case f: Expr.FunctionDef[Id] => Scoped[F, Scope].scope(_.addFunction(f))(Applicative[F].unit)
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
