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

  val clear = "\u001b[2J\u001b[H"
  val p = Paths.get("./example.s")

  import scala.concurrent.duration._
  val sources =
    fs2.Stream.eval(Files[IO].readAll(p, 4096).through(fs2.text.utf8Decode[IO]).compile.string)
      // .repeat.metered(1.second)
      .changes

  import io.circe.syntax._
  val run: IO[Unit] =
    sources.evalMap { source =>
      SourceParser.instance[IO].parse(SourceFile("example.s", source))
        .flatTap(result => IO.println("Parsed program: " ++ result.toString))
        // .flatTap(result => IO(println(result.asJson.noSpaces)))
        .flatMap { expr =>
          IO.println("\n\nProgram output: ") *>
            Interpreter.instance[StateT[IO, Scope, *]].run(expr).runS(Scope.init)
        }
        .flatMap(result => IO.println("\n\nFinal scope: " ++ result.toString))
        .handleErrorWith {
          case Failure.Parsing(t) => IO.println(parser.prettyPrint("example.s", source, t))
        }
    }.compile.drain
