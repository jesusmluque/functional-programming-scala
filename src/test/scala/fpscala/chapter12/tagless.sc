import java.util.UUID
import java.util.concurrent.TimeUnit

//import scalaz._
//Comment the Scalaz object to implement by our self the
//MonadOpts class.
//import Scalaz._
import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

//--------------Monad definition--------------------------
trait Monad[F[_]] {
  def point[A](a: => A):F[A]
  def map[A,B](fa: F[A])(f: A => B):F[B]
  def bind[A,B](fa: F[A])(f:A => F[B]):F[B]
}

//Implicit class to apply the monad methods without implicitly
//With the Ops classes, as a convention, you can apply implicitly monad method in
//directly in the F object
//The context bind is equivalent to
//implicit class monadOps[F[_], A](fa: F[A])(implicit m: Monad[F]) {
//   def flatMap[B](f: A => F[B]): F[B] = m.bind(fa)(f)
//   def map[B](f: A => B): F[B] = m.map(fa)(f)
//}
implicit class monadOps[F[_]: Monad,A](fa: F[A]) {
  def flatMap[B](f: A => F[B]): F[B] = implicitly[Monad[F]].bind(fa)(f)
  def map[B](f: A => B): F[B] = implicitly[Monad[F]].map(fa)(f)

}
//---------------------------------------------------------------------

//-------------------The Algebra and the Program-----------------------
// our business logic, could be
//completley separated from the rest.
case class Person(name: String, id: UUID)
//The Algebra for a repository of persons.
trait PersonRepository[F[_]] {
  def save(person: Person):F[Person]
  def findPerson(id: UUID):F[Option[Person]]
}
//Define the program using an abstract Monad F.
class Program[F[_]: Monad](repo: PersonRepository[F]) {
  def addOne() = {
    val p1 = new Person("Juan", UUID.randomUUID())
    //First version: without MonadOps, we need to use implictly
    //implicitly[Monad[F]].bind(repo.save(p1))(a => repo.findPerson(p1.id))
    //With the MonadOpts implementing only the flatmap, you can avoid implicitly
    //repo.save(p1).flatMap(_ => repo.findPerson(p1.id))
    //If the MonadOpts has flatmap and map, now we can use the for comprehencion
    for {
      _ <- repo.save(p1)
      p <- repo.findPerson(p1.id)
    } yield p
  }

}

//-----------------------------Interpreter------------------------------
//First, we need to create an instance of the monad for the concurrent.Future class
//Type class instance using the above Monad definition:
trait futureInstance extends Monad[Future] {
  override def point[A](a: => A): Future[A] = Future.successful(a)
  override def map[A, B](fa: Future[A])(f: (A) => B): Future[B] = fa.map(f)

  override def bind[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] = fa.flatMap(f)
}

implicit def fInstance:Monad[Future] = new futureInstance {}

//And at the end, we implement the interpreter of our algebra
//using an implementation of a Future Monad.
//Dependency inversion principle applied to FP
//The interpreter depend on the algebra. The implementation
//of the Algebra is an implementation detail, it is not a fundamental
//part of the program and could be easily substituted for any other
//implementation.
trait MapInterpreter extends PersonRepository[Future] {
  val mapkey = new mutable.HashMap[UUID, Person]()
  def save(person: Person):Future[Person] = {
    val newPerson = person.copy()
    mapkey.put(newPerson.id, newPerson)

    Future.successful(newPerson)
  }
  def findPerson(id: UUID):Future[Option[Person]] = {
    Future.successful(mapkey.get(id))
  }
}

//------------------------Injection and Execution---------------------
//Here, at the edge of the progrma,  we inject the interpreter into the program.
val result = new Program(new MapInterpreter {}).addOne()
Await.result(result, Duration(3, TimeUnit.SECONDS))
