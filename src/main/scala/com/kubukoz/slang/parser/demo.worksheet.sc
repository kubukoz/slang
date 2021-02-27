
import cats.parse._
import com.kubukoz.slang.parser.parsing._

println(scala.util.Random.nextInt)

parser.parseAll(
  "42"
).getOrElse(???)

parser.parseAll(
  "def soMuchFun0(arg) = def foo(a) = a"
).getOrElse(???)

parser.parseAll(
  "def identity(arg) = identity(arg)"
).getOrElse(???)

parser.parseAll(
  "def identity(arg) = identity  ( identity(arg))"
).getOrElse(???)


parser.parseAll("def dupa ( fun )  = fun").getOrElse(???)

parser.parseAll("""def identity(arg) = arg

def soMuchFun(arg) = identity(arg)

def evenMoreFun(arg) = soMuchFun(soMuchFun(arg))

println(42)
println(addOne(42))
println( addOne     ( addOne     ( addOne ( 42   ) ) ) )
""")

parser.parseAll("""def identity(arg) = arg

def soMuchFun(arg) = identity(arg)

def evenMoreFun(arg) = soMuchFun ( soMuchFun ( arg))

println ( println )
""")
