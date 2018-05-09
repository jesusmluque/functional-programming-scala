 import fpscala.chapter11.{Comonad, Monad}
 import fpscala.chapter11.Implicits._
 import fpscala.chapter12.Implicits._
 import fpscala.chapter11.ImplicitsCo._

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
val somef: Option[Int => Int => Int => Int] = Some(f)
(Some(1) map f) <*> Some(2) <*> Some(3)
somef <*> Some(1) <*> Some(2) <*> Some(3)
optionM.unit(f) <*> Some(1) <*> Some(2) <*> Some(3)

//Kleisli composition
val fk = ((a:Int) => Option(a + 1)) <=< ((b:Int) => Some(b * 2))
fk(3)

//Monad's laws
val f1 = (a:Int) => Option(a + 1)
val f2 = (a:Int) => Option(a * 2)
val f3 = (a:Int) => Option(a + 3)
//Associativity law:
((f1 <=< f2) <=< f3)(1) == (f1 <=< (f2 <=< f3))(1)
(f1 <=< f2 <=< f3)(1)
//Identity law:
val unit = optionM.unit[Int] _
val unit2 = (a:Int) => Some(a)
(f1 <=< unit2)(1) == f1(1)
(unit <=< f1)(1) == f1(1)


//Comonads instance for Non Empty List
//Definition of NEL:
case class NEL[A](head: A, tail:Option[NEL[A]]) {
  def tails: NEL[NEL[A]] = NEL(this, tail.map(_.tails))
  def getHead = head
  def ::(a: A) = NEL(a, Some(this))
}
//Comonad instance
implicit val nelComonadInstance = new Comonad[NEL] {
  override def extract[A](a: NEL[A]):A = a.getHead
  override def extend[A, B](g: NEL[A] => B)(fa: NEL[A]):NEL[B] = {
    fa match {
      case NEL(h, None) => NEL(g(NEL(h, None)), None)
      case NEL(h, t) => NEL(g(NEL(h, None)), Some(extend(g)(t.get)))
    }
  }
}
//An example of a NEL
val l:NEL[Int] = 3 :: 2 :: NEL(1, None)
//Extend:
l.extend(a => a.head + 1)
l =>> (a => a.head + 1)
((a: NEL[Int]) => a.head + 1) <<= l

val c1 = (a: NEL[Int]) => a.head + 1
val c2 = (a: NEL[Int]) => a.head + 2
(c1 =<= c2)(l)