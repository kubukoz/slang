package com.kubukoz.slang.qualifier

import cats._
import cats.data.NonEmptyList
import cats.data.StateT
import cats.data.Kleisli
import com.kubukoz.slang.ast._
import cats.implicits._
import cats.effect.std.Console

def qualify[F[_]: Console](parsed: Expr[Id])(using MonadError[F, Throwable]): F[Expr[Id]] =
  qualify0[StateT[F, Scope, *]](parsed)
    .runA(Scope.init)

def qualify0[F[_]: Console](parsed: Expr[Id])(
  using MonadError[F, Throwable],
  Scoped[F, Scope]
): F[Expr[Id]] =
  val recurse = qualify0[F](_)
  parsed match
    case lit: Expr.Literal[Id] => lit.pure[F]
    case Expr.Term(name) =>
      Scope
        .ask[F]
        .flatMap {
          _
            .currentNames
            .get(name)
            .liftTo[F](new Throwable("unknown name"))
        }
        .map(Expr.Term[Id])

    case Expr.FunctionDef(functionName, argument, body) =>
      def functionQualified(name: Name): Name =
         Name(s"${functionName.value}(${name.value})")

      // Just in case we have more arguments later (or currying)
      val arguments = List(argument.name).map(arg =>
        arg -> functionQualified(arg)
      ).toMap

      //todo qualify function name itself

      val qualifiedArg = Expr.Argument[Id](functionQualified(argument.name)).pure[F]
      val qualifiedBodyF = recurse(body)

      //so what's happening here is probably
      //a HKT's F can't be inferred because of the extra call at the end
      //this should be easily reproducible
      (
        functionName.pure[F],
        qualifiedArg,
        qualifiedBodyF
      )
        .mapN(Expr.FunctionDef[Id].apply)
        .scope(_.addNames(arguments))


    case literallyAnythingElse =>
      Console[F]
        .errorln("I don't know how to qualify this node: " ++ literallyAnythingElse.toString)
        .as(literallyAnythingElse)

end qualify0

trait Scoped[F[_], S]:
  def ask: F[S]
  def scope[A](f: S => S)(fa: F[A]): F[A]
  extension[A](fa: F[A])
    def scope(f: S => S): F[A] = Scoped.this.scope(f)(fa)

object Scoped:
  def apply[F[_], S](using Scoped[F, S]): Scoped[F, S] = summon
  type Of[S] = [F[_]] =>> Scoped[F, S]

  given [F[_]: Monad, S]: Scoped[StateT[F, S, *], S] = new Scoped[StateT[F, S, *], S]:
    def ask: StateT[F, S, S] = StateT.get
    def scope[A](f: S => S)(fa: StateT[F, S, A]): StateT[F, S, A] = StateT { state =>
      val localState = f(state)

      fa.run(localState)
    }

case class Scope(
  currentPath: List[String],
  currentNames: Map[Name, Name]
):
  // so this is good actually, because we get shadowing for free
  // I guess...
  def addNames(names: Map[Name, Name]): Scope = copy(
    currentNames = currentNames ++ names
  )

object Scope:
  private val builtins = Map("println" -> "<builtins>.println").map {
    (k, v) => Name(k) -> Name(v)
  }

  val init: Scope = Scope(Nil, builtins)
  def ask[F[_]](using Scoped[F, Scope]): F[Scope] = Scoped[F, Scope].ask

