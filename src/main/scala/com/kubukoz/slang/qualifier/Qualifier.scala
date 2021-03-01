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

  def scopedInstance[F[_]: Console](using MonadError[F, Throwable], Scoped[F, Scope], SlangFlags): Qualifier[F] = new Qualifier[F]:

    def useScope[A](f: Scope => F[A]): F[A] = Scope.ask[F].flatMap(f)
    extension[A](fa: F[A])
      def withForkedScope(f: Scope => Scope): F[A] =

        val mkForkScope: F[Scope] =
          useScope(_.pure)
            .map(_.fork.pipe(f))
            .flatTap { forked =>
              val indent = " " * forked.depth * 2

              Console[F].println(s"$indent${forked.debug}").ifDebugM
            }

        Scoped[F, Scope].scope(fa) {
          mkForkScope
        }

    def qualify(parsed: Expr[Id]): F[Expr[Id]] =
      val recurse = qualify
      parsed match
        case lit: Expr.Literal[Id] => lit.pure[F]
        case Expr.Term(name) =>
          useScope { scope =>
            scope
              .currentNames
              .get(name)
              // .getOrElse(Name("<unresolved>."+name.value)).pure[F]
              .liftTo[F](Failure.Qualifying(name, scope))
          }
          .map(Expr.Term[Id])

        case Expr.FunctionDef(functionName, argument, body) =>
          useScope { scope =>
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
  // Note: forkScope can *not* modify the state itself, it only produces it!
  // Even if the state is modified, we will set it again in the implementations.
  def scope[A](fa: F[A])(forkScope: F[S]): F[A]

object Scoped:
  def apply[F[_], S](using Scoped[F, S]): Scoped[F, S] = summon
  type Of[S] = [F[_]] =>> Scoped[F, S]

  given [F[_]: Console, E, S](using MonadError[F, E]): Scoped[StateT[F, S, *], S] = new Scoped[StateT[F, S, *], S]:

    val ask: StateT[F, S, S] = StateT.get

    def scope[A](fa: StateT[F, S, A])(forkScope: StateT[F, S, S]): StateT[F, S, A] = StateT.get[F, S].flatMap { state =>
      StateT.liftF(
        (forkScope.flatMap(StateT.set) *> fa).runA(state)
      )
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

  def debug: String =
    val colors = List[scala.Console.type => String](
      _.MAGENTA,
      _.BLUE,
      _.GREEN,
      _.CYAN,
      _.WHITE
    )

    def inColor(level: Int)(s: String) = colors(level % colors.size)(scala.Console) ++ s ++ scala.Console.RESET

    inColor(depth)(
      s"scope @ ${this.currentPath.reverse.mkString(".")}: symbols ${this.currentLocalNames.keySet.map(_.value).mkString(", ")}"
    )

  def depth: Int = parents.size


object Scope:
  val root: Scope = Scope(Nil, Map.empty, Nil)
  def ask[F[_]](using Scoped[F, Scope]): F[Scope] = Scoped[F, Scope].ask

final case class SlangFlags(debug: Boolean):
  extension[F[_], A](fa: F[A]) def ifDebugM(using Applicative[F]): F[Unit] = fa.whenA(debug)

object SlangFlags:
  given global: SlangFlags = SlangFlags(true)
