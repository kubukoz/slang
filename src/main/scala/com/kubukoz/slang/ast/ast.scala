package com.kubukoz.slang.ast

import com.kubukoz.slang.ast.{Literal => AstLiteral}
import cats.data.NonEmptyList
import cats.Functor
import cats.implicits._

final case class Name(value: String) extends AnyVal:
  def traverse[F[_]: Functor](f: (value: String) => F[String]): F[Name] =
    f(value).map(Name(_))

enum Expr[F[_]]:
  case Literal(value: F[AstLiteral])
  case Term(name: F[Name])
  case Apply(on: Expr[F], param: Expr[F])
  case Block(expressions: NonEmptyList[Expr[F]])
  case FunctionDef(name: F[Name], arg: F[Argument[F]], body: F[Expr[F]])

object Expr:
  def block[F[_]](b1: Expr[F], more: Expr[F]*): Expr.Block[F] = Expr.Block(NonEmptyList(b1, more.toList))

final case class Argument[F[_]](name: F[Name])

enum Literal:
  case Number(value: 42)
