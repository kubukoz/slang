package com.kubukoz.slang.core

import cats.MonadError
import cats.effect.IO
import cats.effect.IOLocal
import cats.data.StateT
import cats.syntax.all._

// A slightly more powerful mtl.Local.
trait Scoped[F[_], S]:
  def ask: F[S]
  // Note: forkScope can *not* modify the state itself, it only produces it!
  // Even if the state is modified, we will set it again in the implementations.
  def scope[A](fa: F[A])(forkScope: F[S]): F[A]

object Scoped:
  def apply[F[_], S](using Scoped[F, S]): Scoped[F, S] = summon
  type Of[S] = [F[_]] =>> Scoped[F, S]

  trait Make[F[_]]:
    def make[S](default: S): F[Scoped[F, S]]

  def make[F[_]: Make, S](default: S): F[Scoped[F, S]] = summon[Make[F]].make(default)

  object Make {

    given Make[IO] with

      def make[S](default: S): IO[Scoped[IO, S]] = IOLocal(default).map { local =>
        new Scoped[IO, S]:
          val ask: IO[S] = local.get
          def scope[A](fa: IO[A])(forkScope: IO[S]): IO[A] = ask.flatMap { oldScope =>
            forkScope.bracket(local.set(_) *> fa)(_ => local.set(oldScope))
          }
      }

  }
