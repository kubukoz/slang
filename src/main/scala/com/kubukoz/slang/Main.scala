package com.kubukoz.slang

import cats.effect.IOApp
import cats.effect.IO
import cats.implicits._
import java.nio.file.Paths
import fs2.io.file.Files
import cats.data.StateT
import com.kubukoz.slang.parser.SourceParser
import com.kubukoz.slang.parser.SourceFile

object Main extends IOApp.Simple:

  val run: IO[Unit] =
    Files[IO].readAll(Paths.get("./example.s"), 4096)
      .through(fs2.text.utf8Decode[IO])
      .compile.string
      .map(SourceFile("example.s", _))
      .flatMap(SourceParser.instance[IO].parse(_))
      .flatTap(IO.println(_))
      .flatMap { expr =>
         Interpreter.instance[StateT[IO, Scope, *]].run(expr).run(Scope.init)
      }
      .flatMap(IO.println(_))
