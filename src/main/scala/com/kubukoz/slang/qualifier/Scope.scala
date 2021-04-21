package com.kubukoz.slang.qualifier

import com.kubukoz.slang._
import com.kubukoz.slang.config.SlangFlags
import com.kubukoz.slang.core.Scoped
import cats.Monad
import com.kubukoz.slang.ast.Name
import cats.syntax.all._
import cats.effect.std.Console
import scala.util.chaining._

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
  ).map { (k, v) =>
    Name(k) -> Name(v)
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

  def renderPath: Option[String] = currentPath.reverse.toNel.map(_.mkString_("."))

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
      s"scope @ ${this.renderPath.orEmpty}: symbols ${this.currentLocalNames.keySet.map(_.value).mkString(", ")}"
    )

  def depth: Int = parents.size

object Scope:
  val root: Scope = Scope(Nil, Map.empty, Nil)

  trait Ops[F[_]]:
    def qualify(name: Name): F[Name]
    def useScope[A](f: Scope => F[A]): F[A]
    extension [A](fa: F[A]) def withForkedScope(f: Scope => Scope): F[A]

  object Ops:
    def apply[F[_]](using Ops[F]): Ops[F] = summon

    given [F[_]: Scoped.Of[Scope]: Console: Monad](
      using SlangFlags
    ): Ops[F] with
      def qualify(name: Name): F[Name] = useScope { scope =>
        Name(scope.renderPath.foldMap(_ ++ ".") + name.value).pure[F]
      }

      def useScope[A](f: Scope => F[A]): F[A] = Scoped[F, Scope].ask.flatMap(f)

      extension [A](fa: F[A])
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

        end withForkedScope
