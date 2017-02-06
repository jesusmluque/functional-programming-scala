package fpscala.chapter12
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class FunctorSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  property("startsWith") { forAll { (a: String, b: String) =>
    (a+b).startsWith(a)}
  }

  property("concatenate") { forAll { (a: String, b: String) =>
    (a+b).length >= a.length && (a+b).length >= b.length
  }}

  property("substring") { forAll { (a: String, b: String, c: String) =>
    (a+b+c).substring(a.length, a.length+b.length) == b
  }}
  val gen:Gen[Option[String]] = for { a <- Gen.alphaStr} yield Some(a)
  property("functor") { forAll(gen) { (a: Option[String]) => {
        val optionF:Functor[Option] = new Functor[Option] {
          override def map[A, B](fa: Option[A])(f: (A) => B):Option[B] = fa map f
        }
        optionF.map(a) { b: String =>
          b + "Pedro"
        }   should be (Some(a.get + "Pedro"))
      }
    }
  }

}