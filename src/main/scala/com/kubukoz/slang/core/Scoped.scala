package com.kubukoz.slang.core

import cats.MonadError
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

  given [F[_], E, S](using MonadError[F, E]): Scoped[StateT[F, S, *], S] = new Scoped[StateT[F, S, *], S]:

    val ask: StateT[F, S, S] = StateT.get

    def scope[A](fa: StateT[F, S, A])(forkScope: StateT[F, S, S]): StateT[F, S, A] = StateT.get[F, S].flatMap { state =>
      StateT.liftF(
        (forkScope.flatMap(StateT.set) *> fa).runA(state)
      )
    }
