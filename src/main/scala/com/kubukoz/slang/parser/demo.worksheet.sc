
import cats.parse._
import com.kubukoz.slang.parser.parsing._

println(scala.util.Random.nextInt)

singleExpression.parseAll(
  "42"
)

singleExpression.parseAll(
  "def identity(arg) = identity(arg)"
)

singleExpression.parseAll(
  "def identity(arg) = identity(identity(arg))"
)
