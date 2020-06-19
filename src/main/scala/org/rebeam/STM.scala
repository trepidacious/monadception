package org.rebeam

import cats.Monad
import Monadception._

object STM {
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
  
  // By providing the ops again as instances of Read, we allow programs to
  // be constructed as a Read directly from basic ops, in a plain for comprehension
  // without the need for a "wrapper function"
  object Read {
    def pure[A](a: A) = new Read[A] {
      def apply[F[_]: Monad](implicit readOps: ReadOps[F]): F[A] = readOps.pure(a)
    }
    def get[A](id: Id[A]): Read[A] = new Read[A] {
      def apply[F[_]: Monad](implicit readOps: ReadOps[F]): F[A] = readOps.get(id)
    }

  }

  // Same again for Edit
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