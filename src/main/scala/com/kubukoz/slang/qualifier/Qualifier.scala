package com.kubukoz.slang.qualifier

import com.kubukoz.slang.Summon1
import com.kubukoz.slang.Failure
import cats.Id
import cats.FlatMap
import cats.MonadError
import cats.data.NonEmptyList
import cats.data.StateT
import cats.effect.std.Console
import cats.effect.IO
import com.kubukoz.slang.ast.*
import cats.syntax.all.*
import com.kubukoz.slang.core.Scoped

def qualify[F[_]: Console: Scoped.Make](parsed: Expr[Id])(using MonadError[F, Throwable]): F[Expr[Id]] =
  Scoped.make[F, Scope](Scope.root).flatMap { qq =>
    given Scoped[F, Scope] = qq
    Qualifier.scopedInstance[F].qualify(parsed)
  }

trait Qualifier[F[_]]:
  def qualify(parsed: Expr[Id]): F[Expr[Id]]

object Qualifier extends Summon1[Qualifier]:

  given [F[_]: FlatMap](
    using Q: Qualifier[StateT[F, Scope, *]]
  ): Qualifier[F] =
    parsed => Q.qualify(parsed).runA(Scope.root)

  def scopedInstance[F[_]: Scope.Ops](using MonadError[F, Throwable]): Qualifier[F] = new Qualifier[F]:

    def qualify(parsed: Expr[Id]): F[Expr[Id]] =
      parsed match
        case lit: Expr.Literal[Id] => lit.pure[F]
        case Expr.Term(name)       =>
          Scope
            .Ops[F]
            .useScope { scope =>
              scope
                .currentNames
                .get(name)
                // .getOrElse(Name("<unresolved>."+name.value)).pure[F]
                .liftTo[F](Failure.Qualifying(name, scope))
            }
            .map(Expr.Term[Id])

        case Expr.FunctionDef(functionName, argument, body) =>
          Scope.Ops[F].qualify(functionName).flatMap { functionNameQualified =>
            def functionQualified(name: Name): Name =
              Name(s"${functionNameQualified.value}(${name.value})")

            // Just in case we have more arguments later (or currying)
            val arguments = List(argument.name).map(arg => arg -> functionQualified(arg)).toMap

            val functionKnownName = Map(functionName -> functionNameQualified)

            // this is safe, trust me 😂
            val qualifiedArg = Argument[Id](arguments(argument.name))

            val qualifiedBodyF = qualify(body)

            (
              functionNameQualified.pure[F],
              qualifiedArg.pure[F],
              qualifiedBodyF
            )
              .mapN(Expr.FunctionDef[Id].apply)
              .withForkedScope(_.addNames(functionKnownName ++ arguments).addPath(functionName.value))
          }

        case Expr.Apply(on, param) =>
          (
            qualify(on),
            qualify(param)
          ).mapN(Expr.Apply.apply)

        // At every block, we go through all the top-level nodes
        // and see if they introduce new symbols. This would be the case for function or constant definitions.
        // If they do, we qualify them here, and create a "virtual" scope for the entire block.
        // This allows functions in a block to see each other, including mutual recursion.
        // todo: deduplicate this with the usual qualification of functions above
        case Expr.Block(nodes) =>
          // todo: add path element for block? Probably have to come up with synthetic IDs at this point
          nodes
            .traverse(prequalify)
            .map(_.toList.flatten.toMap)
            .flatMap { names =>
              nodes
                .traverse(qualify)
                .withForkedScope(_.addNames(names))
            }
            .map(Expr.Block(_))

    end qualify

    private def prequalify: Expr[Id] => F[Map[Name, Name]] =
      case Expr.FunctionDef(functionName, _, _) =>
        Scope.Ops[F].qualify(functionName).map { functionNameQualified =>
          // note: copied verbatim from the functiondef case in qualify0
          // this must be deduplicated (ideally names will be ADTs with scope options)
          Map(functionName -> functionNameQualified)
        }

      case _ =>
        // Not supporting any other means of introducing symbols in blocks yet
        Map.empty.pure[F]

  end scopedInstance

end Qualifier
