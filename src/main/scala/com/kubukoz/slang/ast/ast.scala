package com.kubukoz.slang.ast
import com.kubukoz.slang.ast.{Literal => AstLiteral}
import io.circe._
import cats.data.NonEmptyList

case class Name(value: String)

enum Expr[F[_]]:
  case Literal(value: AstLiteral)
  case Term(name: F[Name])
  case Apply(on: Expr[F], param: Expr[F])
  case Argument(name: F[Name])
  case Block(expressions: NonEmptyList[Expr[F]])
  case FunctionDef(name: F[Name], arg: F[Argument[F]], body: F[Expr[F]])

object Expr:
  given Codec.AsObject[Expr[cats.Id]] = Codec.AsObject.derived

enum Literal derives Codec.AsObject:
  case Number(value: 42)
