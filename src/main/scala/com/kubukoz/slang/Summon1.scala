package com.kubukoz.slang

trait Summon1[Alg[_[_]]]:
  def apply[F[_]](using F: Alg[F]): F.type = F
