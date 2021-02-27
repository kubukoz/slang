package com.kubukoz.slang.qualifier

import com.kubukoz.slang._
import cats._
import cats.data.NonEmptyList
import cats.data.StateT
import cats.data.Chain
import cats.data.Kleisli
import com.kubukoz.slang.ast._
import cats.implicits._
import cats.effect.std.Console

def qualify[F[_]: Console](parsed: Expr[Id])(using MonadError[F, Throwable]): F[Expr[Id]] =
  qualify0[StateT[F, Scope, *]](parsed)
    .runA(Scope.init)

private def qualify0[F[_]: Console](parsed: Expr[Id])(
  using MonadError[F, Throwable],
  Scoped[F, Scope]
): F[Expr[Id]] =
  val recurse = qualify0[F]
  parsed match
    case lit: Expr.Literal[Id] => lit.pure[F]
    case Expr.Term(name) =>
      Scope
        .ask[F]
        .flatMap { scope =>
          scope
            .currentNames
            .get(name)
            .liftTo[F](Failure.Qualifying(name, scope))
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
        .scope(_.addNames(arguments).addPath(functionName.value))

    case Expr.Apply(on, param) =>
      (
        recurse(on),
        recurse(param)
      ).mapN(Expr.Apply.apply)

    case Expr.Block(nodes) =>
      // This is veeeeeery simplified, doesn't account for the fact that previous nodes should be able to
      // influence next blocks... and possibly vice versa! This is going to be some tough stufffffffff.
      nodes.traverse(recurse).map(Expr.Block(_))

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

  given [F[_], E, S](using MonadError[F, E]): Scoped[StateT[F, S, *], S] = new Scoped[StateT[F, S, *], S]:
    def ask: StateT[F, S, S] = StateT.get
    def scope[A](f: S => S)(fa: StateT[F, S, A]): StateT[F, S, A] = StateT { state =>
      fa
        .runA(f(state))
        .map(state -> _)
    }

case class Scope(
  currentPath: Chain[String],
  currentLocalNames: Map[Name, Name]
):
  private val builtins = Map("println" -> "<builtins>.println").map {
    (k, v) => Name(k) -> Name(v)
  }

  def currentNames: Map[Name, Name] = builtins ++ currentLocalNames

  // so this is good actually, because we get shadowing for free
  // I guess...
  def addNames(names: Map[Name, Name]): Scope = copy(
    currentLocalNames = currentLocalNames ++ names
  )

  def addPath(element: String): Scope = copy(
    currentPath = currentPath.append(element)
  )

object Scope:
  val init: Scope = Scope(Chain.nil, Map.empty)
  def ask[F[_]](using Scoped[F, Scope]): F[Scope] = Scoped[F, Scope].ask

