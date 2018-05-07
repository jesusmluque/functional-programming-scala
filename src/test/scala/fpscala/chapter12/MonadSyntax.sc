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
val f = ((_:Int) + (_:Int) + (_:Int)).curried
(Some(1) map f) <*> Some(2) <*> Some(3)
val somef: Option[Int => Int => Int => Int] = Some(f)
somef <*> Some(1) <*> Some(2) <*> Some(3)

//Kleisli composition
val fk = ((a:Int) => Option(a + 1)) <=< ((b:Int) => Some(b * 2))
fk(3)

//Monad's laws
val f1:Int => Option[Int] = (a:Int) => Option(a + 1)
val f2 = (a:Int) => Option(a * 2)
val f3 = (a:Int) => Option(a + 3)
//Associativity law:
((f1 <=< f2) <=< f3)(1) == (f1 <=< (f2 <=< f3))(1)
//Identity law:
val unit = optionM.unit[Int] _
val unit2 = (a:Int) => Some(a)
(f1 <=< unit2)(1) == f1(1)
(unit <=< f1)(1) == f1(1)