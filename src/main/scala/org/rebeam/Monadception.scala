package org.rebeam

import cats.Monad
import cats.implicits._

object Monadception {

  // TODO change to cats trait?
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

  // Now we will define a simple language allowing access to "mutable" cells with Ids
  case class Id[A](guid: Long)

  // Define some operations in tagless style as normal - this is a restricted
  // set of operations for reading state only
  abstract class ReadOps[F[_]: Monad] extends Ops[F]{
      def pure[A](a: A): F[A] = implicitly[Monad[F]].pure(a)
      def get[A](id: Id[A]): F[A]
  }

  // We can extend the ops, from just reading data to also editing it
  abstract class EditOps[F[_]: Monad] extends ReadOps[F] {
      def put[A](create: Id[A] => F[A]): F[Id[A]]
      def modify[A](id: Id[A], f: A => F[A]): F[A]
  }

  // Then we wrap a tagless Read "program" in a trait that allows us to produce that 
  // program in any F for which readOps are available
  // Program.monadInstance provides a Monad instance 
  type Read[A] = Program[A, ReadOps]

  // Now we repeat the wrapping into a Monad for EditOps
  type Edit[A] = Program[A, EditOps]
  
  object Read {
    // By providing the ops again as instances of this monad, we allow programs to
    // be constructed as a Read directly from basic ops, in a plain for comprehension
    // without the need for a "wrapper function"
    def pure[A](a: A) = new Read[A] {
      def apply[F[_]: Monad](implicit readOps: ReadOps[F]): F[A] = readOps.pure(a)
    }
    def get[A](id: Id[A]): Read[A] = new Read[A] {
      def apply[F[_]: Monad](implicit readOps: ReadOps[F]): F[A] = readOps.get(id)
    }

  }

  object Edit {

    // This is unfortunate - we repeat the basic op Monads shared with Read - can we avoid this?
    def pure[A](a: A) = new Edit[A] {
      def apply[F[_]: Monad](implicit editOps: EditOps[F]): F[A] = editOps.pure(a)
    }

    def get[A](id: Id[A]): Edit[A] = new Edit[A] {
      def apply[F[_]: Monad](implicit editOps: EditOps[F]): F[A] = editOps.get(id)
    }

    def put[A](create: Id[A] => Edit[A]): Edit[Id[A]] = new Edit[Id[A]] {
      def apply[F[_]: Monad](implicit editOps: EditOps[F]): F[Id[A]] = editOps.put(id => create(id).apply[F])
    }

    def modify[A](id: Id[A], f: A => Edit[A]): Edit[A] = new Edit[A] {
      def apply[F[_]: Monad](implicit editOps: EditOps[F]): F[A] = editOps.modify(id, a => f(a).apply[F])
    }

  }

}