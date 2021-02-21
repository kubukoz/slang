
import cats.parse._
import com.kubukoz.slang.parser.parsing._

println(scala.util.Random.nextInt)

parser.parseAll(
  "42"
)

parser.parseAll(
  "def identity(arg) = identity(arg)"
)

parser.parseAll(
  "def identity(arg) = identity  ( identity(arg))"
)


parser.parseAll("def dupa ( fun )  = fun")

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

