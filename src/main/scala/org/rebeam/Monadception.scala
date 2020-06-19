package org.rebeam

import cats.Monad
import cats.implicits._

object Monadception {

  // TODO change to alley-cats Pure trait?
  trait Ops[F[_]] {
    def pure[A](a: A): F[A]
  }

  trait Program[A, O[F[_]] <: Ops[F]] {
    def apply[F[_]: Monad](implicit ops: O[F]): F[A]
  }

  object Program {
    implicit def monadInstance[O[F[_]] <: Ops[F]]: Monad[Program[*, O]] = new Monad[Program[*, O]] {
      override def pure[A](x: A): Program[A, O] = new Program[A, O] {
        def apply[F[_]: Monad](implicit ops: O[F]): F[A] = ops.pure(x)
      }
      override def flatMap[A, B](fa: Program[A, O])(f: A => Program[B, O]): Program[B, O] = new Program[B, O] {
        def apply[F[_]: Monad](implicit ops: O[F]): F[B] = fa[F].flatMap(a => f(a)[F])
      }
      // TODO: Is this stack safe? Not tail recursive, but F's tailRecM should be?
      override def tailRecM[A, B](a: A)(f: A => Program[Either[A,B], O]): Program[B, O] = {
        new Program[B, O] {
          override def apply[F[_] : Monad](implicit ops: O[F]): F[B] = {
            implicitly[Monad[F]].tailRecM(a)(a => f(a).apply[F])
          }
        }
      }
    }
  }

  object Implicits {
    // We can treat a Program[A, X] as a Program[A, Y] if Y <: X, i.e. if the operations in X are all contained in Y because Y is a subclass of X
    implicit class CompatibleProgram[A, X[F[_]] <: Ops[F], Y[F[_]] <: X[F]](program: Program[A, X]) extends Program[A, Y] {
      def apply[F[_]: Monad](implicit ops: Y[F]): F[A] = program.apply[F](implicitly[Monad[F]], ops)
    }
  }

}