package com.kubukoz.slang.parser

import cats.parse.Parser
import cats.parse.Numbers
import cats.parse.LocationMap
import cats.parse.Parser.Expectation
import cats.data.NonEmptyList
import cats.syntax.all._

def prettyPrint(fileName: String, source: String, e: Parser.Error): String =
  def colorize(color: String)(text: String) = text.linesWithSeparators.map(color ++ _).mkString

  val (codeBefore, codeAfter) = source.splitAt(e.failedAtOffset).bimap(colorize(scala.Console.GREEN), colorize(scala.Console.RED))

  def expected(s: String) = s"Expected $s"
  val explainExpectation: Expectation => String =
    case Expectation.FailWith(_, msg) => msg
    case Expectation.OneOfStr(_, strings) => expected(s"one of (${strings.mkString("")})")
    case Expectation.InRange(_, from, to) if from == to => expected(from.toString)
    // does this even make sense?
    case Expectation.InRange(_, from, to) => expected(s"one of ($from..$to)")
    case Expectation.EndOfString(_, _) => expected(s"end of file")
    case e => e.toString ++ " (unsupported failure)"

  val failureString = e.expected match
    case NonEmptyList(one, Nil) => explainExpectation(one)
    case more => "one of: " ++ more.map(explainExpectation).mkString_(", ")

    val lm = LocationMap(source)
    val location = lm.toLineCol(e.failedAtOffset).fold(s"offset ${e.failedAtOffset}") {
      case (line, column) => s"""line $line, column $column""".stripMargin
    }

  val totalLines = source.linesIterator.size
  val lineCountLength = Math.log10(totalLines).toInt + 1
  val allCode = s"""$codeBefore😡 $codeAfter""".linesWithSeparators.zipWithIndex.map {
    case (line, index) => s"""%${lineCountLength}d | $line""".format(index + 1)
  }.mkString

  s"""${"😭" * 10}
      |Unexpected parsing error in file ${scala.Console.MAGENTA}$fileName${scala.Console.RESET} at $location:
      |${scala.Console.RED}$failureString${scala.Console.RESET}
      |
      |Context:
      |$allCode""".stripMargin
end prettyPrint
