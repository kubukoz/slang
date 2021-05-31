package com.kubukoz.slang

import cats.effect.IOApp
import cats.effect.IO
import cats.implicits.*
import java.nio.file.Paths
import fs2.io.file.Files
import cats.data.StateT
import com.kubukoz.slang.interpreter.Interpreter
import com.kubukoz.slang.parser.SourceParser
import com.kubukoz.slang.parser.SourceFile

object Main extends IOApp.Simple:

  val clear = "\u001b[2J\u001b[H"
  val p = Paths.get("./example.sp")

  import scala.concurrent.duration.*

  val sources =
    fs2
      .Stream
      .eval(Files[IO].readAll(p, 4096).through(fs2.text.utf8Decode[IO]).compile.string)
      // .repeat.metered(1.second)
      .changes

  val run: IO[Unit] =
    sources
      .evalMap { source =>
        SourceParser
          .instance[IO]
          .parse(SourceFile("example.sp", source))
          .flatTap(result => IO.println("Parsed program: " ++ result.toString))
          .flatMap(qualifier.qualify(_))
          .flatTap(result => IO.println("Qualified program: " ++ result.toString))
          // .flatTap(result => IO(println(result.asJson.noSpaces)))
          // .flatMap { expr =>
          //   IO.println("\n\nProgram output: ") *>
          //     Interpreter.instance[StateT[IO, interpreter.Scope, *]].run(expr).runS(interpreter.Scope.init)
          // }
          // .flatMap(result => IO.println("\n\nFinal scope: " ++ result.toString))
          .handleErrorWith {
            case Failure.Parsing(t)              => IO.println(parser.prettyPrint("example.sp", source, t))
            case Failure.Qualifying(name, scope) =>
              IO.println(
                // todo: this should also have a location and some surrounding source code
                // todo2: non-fatal errors anyone? we'll need this for the language server
                // shouldn't be too difficult to resolve everything to a nonexistent symbol of the Nothing type or something
                s"""Can't resolve name: ${name.value} in scope ${scope.renderPath.orEmpty}.
                   |Current names:
                   |${scope.currentNames.toList.sortBy(_._1).map((k, v) => s"${k.value} -> ${v.value}").mkString("\n")}""".stripMargin
              )
          }
      }
      .compile
      .drain
end Main
