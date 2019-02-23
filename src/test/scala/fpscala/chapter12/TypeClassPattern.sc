trait Show[A] {
  def show(a: A): String
}

object Show {
  def apply[A: Show](a: A) = show(a)
  def show[A: Show](a: A): String = implicitly[Show[A]].show(a)

  def instance[A](f: A => String) = new Show[A] {
    def show(a: A)= f(a)
  }

  implicit class showOps[A: Show](a: A) {
    def show: String = Show.show(a)
  }

  //Adding instance type class for any case class using Generics from Shapeless.
  import shapeless._

  //Show type class instance for HList generic type, first for HNil, then for any HList
  implicit def hnilShow = new Show[HNil] {
    override def show(a: HNil) = ""
  }

  implicit def hlistShow[H, T <: HList](implicit headShow: Show[H], tailShow: Show[T]) =
    new Show[H :: T] {
      override def show(l: H :: T) = headShow.show(l.head) + ", " + tailShow.show(l.tail)
    }

  //Show type class instance for a case class transforming it in a generic HList using shapeless type class instance,
  //then with the previous Show instance for HList, apply the show method to the generic HList
  implicit def caseClassShow[T, L <: HList](implicit generic: Generic.Aux[T, L], lShow: Show[L]): Show[T] =
    new Show[T] {
      override def show(a: T): String = lShow.show(generic.to(a))
    }
}

import Show._
implicit val intShow = instance((a:Int) => s"Int=$a")
implicit val stringShow = instance((a: String) => s"String=$a")

4.show
Show(5)

case class Person(age: Int, name: String)

Show(Person(5, "Carlos"))
Person(4, "Juan").show




//type classes with simulacrum macros
import simulacrum._
@typeclass trait Number[A] {
  @op("+") def sum(a: A, b: A):A
  @op("*") def times(a: A, b: A): A
  def negate(a:A): A
}

case class Complex(r: Double, i: Double)

implicit val complexNumber = new Number[Complex] {
  override def sum(a: Complex, b: Complex) = Complex(a.r + b.r, a.i + b.i)

  override def times(a: Complex, b: Complex) = Complex(a.r * b.r - a.i * b.i, a.i * b.r + a.i * b.r)

  override def negate(a: Complex) = Complex(a.r * -1, a.i * -1)
}
import Number.ops._
Complex(1,2) + Complex(3, 4)
Complex(1,2) * Complex(3,4)

