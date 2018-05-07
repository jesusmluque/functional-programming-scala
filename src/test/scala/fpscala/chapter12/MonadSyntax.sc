 import fpscala.chapter11.Monad
import fpscala.chapter11.Implicits._
import fpscala.chapter12.Implicits._

implicit val optionM = new Monad[Option] {
  override def unit[A](a: => A):Option[A] = Some(a)
  override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa flatMap(f)
}

val value1: Option[Int] = Some(4)
val value2: Option[Int] = Some(2)

value1 bind {a => Some(a + 1)}
value1 >>= {a => Some(a + 1)}

//A monad is also an Applicative
(value1 |@| value2) {_ + _}
(Some(1) map ((_:Int) + (_:Int) + (_:Int)).curried) <*> Some(2) <*> Some(3)

//Kleisli composition
val fk = ((a:Int) => Option(a + 1)) <=< ((b:Int) => Some(b * 2))
fk(3)