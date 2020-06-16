package org.rebeam

import cats.implicits._
import MapImpl._
import cats.Monad

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

  // We can easily include a Read in an Edit
  def createAndPrint: Edit[String] = {
    import Edit._
    for {
      person <- createPerson
      print <- read(personAndFriend(person.id))
    } yield print
  }

  // We can run apply an Edit with any F for which we have EditOps, in this case we use MapState
  // Right(Person Bob and friend Some(Alice))
  println(createAndPrint[MapState].run(StateData.empty).map(_._2))

  // We can use Read directly as an Edit with Implicits
  def createAndPrintImplicit: Edit[String] = {
    import Edit._
    import Implicits._
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
    import Edit._
    for {
      answer <- new Edit[Int] {
        def apply[F[_]: Monad](implicit editOps: EditOps[F]): F[Int] = doSomething[F]
      }
    } yield answer
  }

  // TODO test monad laws?

}
