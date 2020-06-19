package org.rebeam

import STM._
import cats.implicits._
import cats.data.StateT

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
