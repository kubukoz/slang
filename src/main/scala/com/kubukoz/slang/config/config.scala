package com.kubukoz.slang.config

import cats.Applicative
import cats.syntax.all._

final case class SlangFlags(debug: Boolean):
  extension [F[_], A](fa: F[A]) def ifDebugM(using Applicative[F]): F[Unit] = fa.whenA(debug)

object SlangFlags:
  given global: SlangFlags = SlangFlags(debug = true)
