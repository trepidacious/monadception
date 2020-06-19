package org.rebeam

import cats.implicits._
import cats.Monad

import Monadception._
import Implicits._
import MapImpl._

object Main extends App {

  case class Person(id: Id[Person], name: String, friend: Option[Id[Person]])

  // This can perform edits - put and modify cells
  val createPerson: Edit[Person] = {
    import Edit._
    for {
      aliceId <- put[Person](id => pure(Person(id, "Alice", None)))
      bobId <- put[Person](id => pure(Person(id, "Bob", Some(aliceId))))
      bob <- get(bobId)
    } yield bob
  }

  // This is known only to read data - get cells
  def personAndFriend(id: Id[Person]): Read[String] = {
    import Read._
    for {
      person <- get(id)
      friend <- person.friend.traverse(get)
    } yield s"Person ${person.name} and friend ${friend.map(_.name)}"
  }

  // We can use Read directly as an Edit, using Implicits.ReadAsEdit
  def createAndPrintImplicit: Edit[String] = {
    for {
      person <- createPerson
      print <- personAndFriend(person.id)
    } yield print
  }

  // Right(Person Bob and friend Some(Alice))
  println(createAndPrintImplicit[MapState].run(StateData.empty).map(_._2))

  // We can also use as plain tagless, calling an Edit from this
  def doSomething[F[_]: Monad](implicit editOps: EditOps[F]): F[Int] = {
    import editOps._
    for {
      _ <- createPerson[F]
      id <- put[Int](_ => pure(42))
      answer <- get(id)
    } yield answer
  }

  // Right(42)
  println(doSomething[MapState].run(StateData.empty).map(_._2))

  // Calling the plain tagless from an Edit is inconvenient
  val double: Edit[Int] = {
    for {
      answer <- new Edit[Int] {
        def apply[F[_]: Monad](implicit editOps: EditOps[F]): F[Int] = doSomething[F]
      }
    } yield answer
  }

}
