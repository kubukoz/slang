package com.kubukoz.slang.parser

import cats.parse.Parser
import cats.parse.Numbers
import cats.parse.LocationMap
import cats.parse.Parser.Expectation
import cats.data.NonEmptyList
import cats.syntax.all._

def prettyPrint(fileName: String, source: String, e: Parser.Error): String =
  //uber hack - apparently reset doesn't work after a newline... so we're printing the newline after resetting.
  def colorize(color: String)(text: String) = text.linesWithSeparators.map(s => color ++ s.dropRight(1) ++ Console.RESET ++ s.takeRight(1)).mkString

  val (codeBefore, codeAfter) = source.splitAt(e.failedAtOffset).bimap(colorize(Console.GREEN), colorize(Console.RED))

  def expected(s: String) = s"Expected $s."
  val explainExpectation: Expectation => String =
    case Expectation.FailWith(_, msg) => msg
    case Expectation.OneOfStr(_, List(one)) => expected(one)
    case Expectation.OneOfStr(_, strings) => expected(s"one of [${strings.mkString(", ")}]")
    case Expectation.InRange(_, from, to) if from == to => expected(from.toString)
    case Expectation.InRange(_, from, to) => expected(s"a character from range [$from..$to]")
    case Expectation.EndOfString(_, _) => expected(s"end of file")
    case e => e.toString ++ " (unsupported failure)"

  val failureString = e.expected match
    case NonEmptyList(one, Nil) => explainExpectation(one)
    case more => "Failed expectations:\n" ++ more.map(s => "- " ++ explainExpectation(s)).mkString_("\n")

    val lm = LocationMap(source)
    val location = lm.toLineCol(e.failedAtOffset).fold(s"offset ${e.failedAtOffset}") {
      case (line, column) => s"""line $line, column $column""".stripMargin
    }

  // Counting last empty line
  val totalLines = source.linesWithSeparators.size + 1
  val lineCountLength = Math.log10(totalLines).toInt + 1
  val allCode = s"""$codeBefore😡 $codeAfter""".linesWithSeparators.zipWithIndex.map {
    case (line, index) => s"""%${lineCountLength}d | $line""".format(index + 1)
  }.mkString

  def unescape(ch: Char) = Map(
    '\n' -> "'\\n' (line feed)",
    '\r' -> "'\\r' (carriage return)",
    '\t' -> "'\\t' (tab)"
  ).getOrElse(ch, s"'$ch'")

  s"""${"😭" * 10}
      |Unexpected parsing error in file ${Console.MAGENTA}$fileName${Console.RESET} at $location:
      |${Console.RED}$failureString${Console.RESET}
      |Next char: ${unescape(source(e.failedAtOffset))}
      |
      |Context:
      |$allCode""".stripMargin
end prettyPrint
