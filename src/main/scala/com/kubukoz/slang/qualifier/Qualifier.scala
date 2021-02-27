package com.kubukoz.slang.qualifier

import cats._
import cats.data.NonEmptyList
import com.kubukoz.slang.ast._
import cats.implicits._

val builtins = Map("println" -> "<builtins>.println").map { (k, v) => Name(k) -> Name(v) }

def qualify[F[_]](parsed: Expr[Id], localNames: Map[Name, Name] = builtins)(
  using MonadError[F, Throwable]
): F[Expr[Id]] =
  parsed match
    case lit: Expr.Literal[Id] => lit.pure[F]
    case Expr.Term(name) =>
      localNames
        .get(name)
        .liftTo[F](new Throwable("unknown name"))
        .map(Expr.Term[Id])

    case Expr.FunctionDef(functionName, argument, body) =>
      println(functionName)
      println(argument)
      println(body)

      def functionQualified(name: Name): Name =
         Name(s"${functionName.value}(${name.value})")

      // Just in case we have more arguments later (or currying)
      val arguments = List(argument.name).map(arg =>
        arg -> functionQualified(arg)
      ).toMap

      //todo qualify function name itself

      val qualifiedArg = Expr.Argument[Id](functionQualified(argument.name)).pure[F]
      val qualifiedBodyF = qualify[F](body, localNames ++ arguments)

      (qualifiedArg, qualifiedBodyF).mapN(Expr.FunctionDef(functionName, _, _))

@main def demo = ???
