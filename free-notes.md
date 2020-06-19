import cats._, data._, implicits._
import cats.free._

object Library {
  case class Id[A](guid: Long)

  sealed trait ReadOps[A]
  object ReadOps {
    implicit def reifyRead: Read.Api[Read] = new Read.Api[Read] {
      def get[A](id: Id[A]): Read[A] = Read.get(id)
    }
  }

  sealed trait EditOps[A]
  case class Get[A](a: Id[A]) extends ReadOps[A] with EditOps[A]
  case class Put[A](create: Id[A] => Edit[A]) extends EditOps[Id[A]]
  case class Modify[A](id: Id[A], f: A => Edit[A]) extends EditOps[A]
  object EditOps {
    implicit def embedReadInEdit[A](f: Read[A]): Edit[A] =
      f.mapK {
        new (ReadOps ~> EditOps) {
          def apply[B](a: ReadOps[B]) = a match {
            case Get(id) => Get(id)
          }
        }
      }

    implicit def reifyEdit: Edit.Api[Edit] = new Edit.Api[Edit] {
      def get[A](id: Id[A]): Edit[A] = Read.get(id)
      def put[A](create: Id[A] => Edit[A]): Edit[Id[A]] =
        Edit.put(create)
      def modify[A](id: Id[A], f: A => Edit[A]): Edit[A] =
        Edit.modify(id, f)
    }
  }

  // note: if you need  implicits, put them in the companions of ReadOps/EditOps
  type Read[A] = Free[ReadOps, A]
  object Read {
    def get[A](id: Id[A]): Read[A] = Free.liftF(Get(id))

    trait Api[F[_]] {
      def get[A](id: Id[A]): F[A]
    }
  }

  type Edit[A] = Free[EditOps, A]
  object Edit {
    def put[A](create: Id[A] => Edit[A]): Edit[Id[A]] =
      Free.liftF(Put(create): EditOps[Id[A]])

    def modify[A](id: Id[A], f: A => Edit[A]): Edit[A] =
      Free.liftF(Modify(id, f))

    def get[A](id: Id[A]): Edit[A] = Read.get(id)

    trait Api[F[_]] extends Read.Api[F] {
      def put[A](create: Id[A] => F[A]): F[Id[A]]
      def modify[A](id: Id[A], f: A => F[A]): F[A]
    }
  }

  def liftRead[F[_]: Monad, A](p: Read[A])(implicit F: Read.Api[F]): F[A] =
    p.foldMap {
      new (ReadOps ~> F) {
        def apply[B](p: ReadOps[B]) = p match {
          case Get(id) => F.get(id)
        }
      }
    }

  def liftEdit[F[_]: Monad, A](
      p: Edit[A]
  )(implicit F: Edit.Api[F]): F[A] =
    p.foldMap {
      new (EditOps ~> F) {
        def apply[B](p: EditOps[B]): F[B] = p match {
          case Get(id) => F.get(id)
          case put: Put[x] =>
            F.put { id: Id[x] =>
              liftEdit[F, x](put.create(id))
            }
          case Modify(id, f) =>
            F.modify(
              id,
              (b: B) => liftEdit[F, B](f(b))
            )
        }
      }
    }

  object Interpreter {
    // we could use `foldMap` on free, or convert to tagless and go from there,
    // not important here
  }
}
object Programs {
  import Library._
  case class Person(id: Id[Person], name: String, friend: Option[Id[Person]])

  // This can perform edits - put and modify cells
  val createPerson: Edit[Person] = {
    import Edit._
    for {
      aliceId <- put[Person](id => Person(id, "Alice", None).pure[Edit])
      bobId <- put[Person](id => Person(id, "Bob", Some(aliceId)).pure[Edit])
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
    for {
      person <- createPerson
      print <- personAndFriend(person.id)
    } yield print
  }

  // using Edit in tagless programs
  def doSomething[F[_]: Monad](implicit editOps: Edit.Api[F]): F[Int] = {
    import editOps._
    for {
      _ <- liftEdit[F, Person](createPerson)
      id <- put[Int](_ => 42.pure[F])
      answer <- get(id)
    } yield answer
  }

  // using Read in tagless programs
  def ex1[F[_]: Read.Api: Monad, A](a: Read[A]): F[A] = liftRead[F, A](a)
  def ex2[F[_]: Edit.Api: Monad, A](a: Read[A]): F[A] = liftEdit[F, A](a)

  // including tagless in Edit
  val double: Edit[Int] =
    for {
      answer <- doSomething[Edit]
    } yield answer

  val testId = Id[Person](3)
  def ex3[A]: Read[String] = ex1[Read, String](personAndFriend(testId))
  def ex4[A]: Edit[String] = ex1[Edit, String](personAndFriend(testId))
}