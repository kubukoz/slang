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
import scala.util.chaining._

def qualify[F[_]: Console](parsed: Expr[Id])(using MonadError[F, Throwable]): F[Expr[Id]] =
  given Q: Qualifier[StateT[F, Scope, *]] = Qualifier.scopedInstance
  Qualifier[F].qualify(parsed)

trait Qualifier[F[_]]:
  def qualify(parsed: Expr[Id]): F[Expr[Id]]

object Qualifier:
  def apply[F[_]](using Qualifier[F]): Qualifier[F] = summon
  given[F[_]: FlatMap](using Q: Qualifier[StateT[F, Scope, *]]): Qualifier[F] =
    parsed => Q.qualify(parsed).runA(Scope.root)

  def scopedInstance[F[_]: Console](using MonadError[F, Throwable], Scoped[F, Scope]): Qualifier[F] = new Qualifier[F]:

    extension[A](fa: F[A])
      def withForkedScope(f: Scope => Scope): F[A] = Scoped[F, Scope].scope(_.fork.pipe(f))(fa)

    def qualify(parsed: Expr[Id]): F[Expr[Id]] =
      val recurse = qualify
      parsed match
        case lit: Expr.Literal[Id] => lit.pure[F]
        case Expr.Term(name) =>
          Scope
            .ask[F]
            .flatMap { scope =>
              scope
                .currentNames
                .get(name)
                // .getOrElse(Name("<unresolved>."+name.value)).pure[F]
                .liftTo[F](Failure.Qualifying(name, scope))
            }
            .map(Expr.Term[Id])

        case Expr.FunctionDef(functionName, argument, body) =>
          Scope.ask[F].flatMap { scope =>
            val scopePathPrefix = scope.currentPath.reverse.toNel.fold("")(_.mkString_(".") + ".")
            val functionNameQualified = Name(scopePathPrefix + functionName.value)
            def functionQualified(name: Name): Name =
                Name(s"${functionNameQualified.value}(${name.value})")

            // Just in case we have more arguments later (or currying)
            val arguments = List(argument.name).map(arg =>
              arg -> functionQualified(arg)
            ).toMap

            val functionKnownName = Map(functionName -> functionNameQualified)

            //todo qualify function name itself

            // this is safe, trust me 😂
            val qualifiedArg = Argument[Id](arguments(argument.name))

            val qualifiedBodyF = recurse(body)

            (
              functionNameQualified.pure[F],
              qualifiedArg.pure[F],
              qualifiedBodyF
            )
              .mapN(Expr.FunctionDef[Id].apply)
              .withForkedScope(_.addNames(functionKnownName ++ arguments).addPath(functionName.value))
            }

        case Expr.Apply(on, param) =>
          (
            recurse(on),
            recurse(param)
          ).mapN(Expr.Apply.apply)

        // At every block, we go through all the top-level nodes
        // and see if they introduce new symbols. This would be the case for function or constant definitions.
        // If they do, we qualify them here, and create a "virtual" scope for the entire block.
        // This allows functions in a block to see each other, including mutual recursion.
        // todo: deduplicate this with the usual qualification of functions above
        case Expr.Block(nodes) =>
          // todo: add path element for block? Probably have to come up with synthetic IDs at this point
          nodes
            .traverse(prequalify)
            .map(_.toList.flatten.toMap)
            .flatMap { names =>
              nodes
                .traverse(recurse)
                .withForkedScope(_.addNames(names))
            }.map(Expr.Block(_))

    end qualify

    private def prequalify: Expr[Id] => F[Map[Name, Name]] =
      case Expr.FunctionDef(functionName, _, _) =>
        Scope.ask[F].map { scope =>
          // note: these two lines have been copied verbatim from the functiondef case in qualify0
          // this must be deduplicated (ideally names will be ADTs with scope options)
          val scopePathPrefix = scope.currentPath.reverse.toNel.fold("")(_.mkString_(".") + ".")
          val functionNameQualified = Name(scopePathPrefix + functionName.value)
          Map(functionName -> functionNameQualified)
        }

      case _ =>
        // Not supporting any other means of introducing symbols in blocks yet
        Map.empty.pure[F]

  end scopedInstance

end Qualifier


trait Scoped[F[_], S]:
  def ask: F[S]
  def scope[A](f: S => S)(fa: F[A]): F[A]

object Scoped:
  def apply[F[_], S](using Scoped[F, S]): Scoped[F, S] = summon
  type Of[S] = [F[_]] =>> Scoped[F, S]

  given [F[_]: Console, E, S: Debug: Depth](using MonadError[F, E], SlangFlags): Scoped[StateT[F, S, *], S] = new Scoped[StateT[F, S, *], S]:
    given M: Monad[StateT[F, S, *]] = summon

    def ask: StateT[F, S, S] = StateT.get

    def scope[A](f: S => S)(fa: StateT[F, S, A]): StateT[F, S, A] = StateT { state =>
      def indent(s: S) = " " * s.depth * 2

      val forked = f(state)

      Console[F].println(s"${indent(forked)}${forked.debug}").ifDebugM *>
        fa.runA(forked)
        .map(state -> _)
    }

case class Scope(
  //closest on top
  currentPath: List[String],
  currentLocalNames: Map[Name, Name],
  //closest is on top
  parents: List[Scope]
):
  private val builtins = Map(
    "println" -> "<builtins>.println",
    "addOne" -> "<builtins>.addOne",
    "currentTime" -> "<builtins>.currentTime"
  ).map {
    (k, v) => Name(k) -> Name(v)
  }

  def fork: Scope = copy(parents = this :: parents)
  def currentNames: Map[Name, Name] = builtins ++ currentLocalNames

  // so this is good actually, because we get shadowing for free
  // I guess...
  def addNames(names: Map[Name, Name]): Scope = copy(
    currentLocalNames = currentLocalNames ++ names
  )

  def addPath(element: String): Scope = copy(
    currentPath = element :: currentPath
  )

object Scope:
  val root: Scope = Scope(Nil, Map.empty, Nil)
  def ask[F[_]](using Scoped[F, Scope]): F[Scope] = Scoped[F, Scope].ask

  given Debug[Scope] = scope =>
    val colors = List[scala.Console.type => String](
      _.MAGENTA,
      _.BLUE,
      _.GREEN,
      _.CYAN,
      _.WHITE
    )

    def inColor(level: Int)(s: String) = colors(level % colors.size)(scala.Console) ++ s ++ scala.Console.RESET

    inColor(scope.depth)(
      s"scope @ ${scope.currentPath.reverse.mkString(".")}: symbols ${scope.currentLocalNames.keySet.map(_.value).mkString(", ")}"
    )

  given Depth[Scope] = _.parents.size


trait Debug[A]:
  extension(a: A) def debug: String

trait Depth[A]:
  extension(a: A) def depth: Int

final case class SlangFlags(debug: Boolean):
  extension[F[_], A](fa: F[A]) def ifDebugM(using Applicative[F]): F[Unit] = fa.whenA(debug)

object SlangFlags:
  given global: SlangFlags = SlangFlags(true)
