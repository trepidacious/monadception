package org.rebeam

import cats.Monad
import cats.implicits._
import cats.data.StateT

case class Id[A](guid: Long)

// Define some operations in tagless style as normal
abstract class ReadOps[F[_]: Monad] {
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
trait Read[A] {
  def apply[F[_]: Monad](implicit readOps: ReadOps[F]): F[A]
}

object Read {

  // Read is a Monad, passing through to the underlying Monad used by ReadOps - this
  // is a purely mechanical process
  lazy implicit val monadInstance: Monad[Read] = new Monad[Read] {
    override def pure[A](x: A): Read[A] = Read.pure(x)
    override def flatMap[A, B](fa: Read[A])(f: A => Read[B]): Read[B] = new Read[B] {
      def apply[F[_]: Monad](implicit readOps: ReadOps[F]): F[B] = fa[F].flatMap(a => f(a)[F])
    }
    // TODO: Is this stack safe? Not tail recursive, but F's tailRecM should be?
    override def tailRecM[A, B](a: A)(f: A => Read[Either[A,B]]): Read[B] = {
      new Read[B] {
        override def apply[F[_] : Monad](implicit readOps: ReadOps[F]): F[B] = {
          implicitly[Monad[F]].tailRecM(a)(a => f(a).apply[F])
        }
      }
    }
  }

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

// Now we repeat the wrapping into a Monad for EditOps
trait Edit[A] {
  def apply[F[_]: Monad](implicit editOps: EditOps[F]): F[A]
}

object Edit {

  // Looks exacly like Read, just using Edit and EditOps
  lazy implicit val monadInstance: Monad[Edit] = new Monad[Edit] {
    override def pure[A](x: A): Edit[A] = Edit.pure(x)
    override def flatMap[A, B](fa: Edit[A])(f: A => Edit[B]): Edit[B] = new Edit[B] {
      def apply[F[_]: Monad](implicit editOps: EditOps[F]): F[B] = fa[F].flatMap(a => f(a)[F])
    }
    override def tailRecM[A, B](a: A)(f: A => Edit[Either[A,B]]): Edit[B] = {
      new Edit[B] {
        override def apply[F[_] : Monad](implicit editOps: EditOps[F]): F[B] = {
          implicitly[Monad[F]].tailRecM(a)(a => f(a).apply[F])
        }
      }
    }
  }

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

  def read[A](r: Read[A]): Edit[A] = new Edit[A] {
    def apply[F[_]: Monad](implicit editOps: EditOps[F]): F[A] = r[F]
  }

}

// We can implicitly treat a Read as an Edit if needed
object Implicits {
  implicit class ReadAsEdit[A](read: Read[A]) extends Edit[A] {
    def apply[F[_]: Monad](implicit editOps: EditOps[F]): F[A] = read[F]
  }
}

// A simple implementation of EditOps using a Map - this isn't interesting, just as we
// would expect for tagless final
object MapImpl {
  case class Error(msg: String)

  case class StateData (
    nextGuid: Long,
    map: Map[Long, Any]
  ) {
    def updated[A](id: Id[A], a: A): StateData = {
      copy(map = map.updated(id.guid, a))
    }
  }

  object StateData {
    val empty = StateData(0, Map.empty)
  }

  type ErrorOr[A] = Either[Error, A]

  type MapState[A] = StateT[ErrorOr, StateData, A]

  implicit val editOps: EditOps[MapState] = new EditOps[MapState] {

    private def newGuid: MapState[Long] =
      StateT[ErrorOr, StateData, Long](sd => {
        Right(
          (
            sd.copy(nextGuid = sd.nextGuid + 1),
            sd.nextGuid
          )
        )
      })

    private def set[A](id: Id[A], a: A): MapState[Unit] = for {
      _ <- StateT.modify[ErrorOr, StateData](s => s.updated(id, a))
    } yield ()

    def get[A](id: Id[A]): MapState[A] = StateT.inspectF[ErrorOr, StateData, A](
      _.map.get(id.guid)
        .map(_.asInstanceOf[A])
        .toRight(Error(s"No data for $id"))
    )

    def put[A](create: Id[A] => MapState[A]): MapState[Id[A]] = for {
      id <- newGuid.map(Id[A](_))
      a <- create(id)
      _ <- set(id, a)
    } yield id

    def modify[A](id: Id[A], f: A => MapState[A]): MapState[A] = for {
      a <- get(id)
      newA <- f(a)
      _ <- set(id, newA)
    } yield newA
  }

}



