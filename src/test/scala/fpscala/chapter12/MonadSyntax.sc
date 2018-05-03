import fpscala.chapter11.Monad
import fpscala.chapter11.Implicits._
import fpscala.chapter12.Implicits._

implicit val optionM = new Monad[Option] {
  def unit[A](a: => A):Option[A] = Some(a)
  override def join[A](ffa: Option[Option[A]]):Option[A] = ffa.get
  override def map[A,B](fa: Option[A])(f: A => B) = fa map f
  override def ap[A,B](fab: Option[A => B])(fa: Option[A]):Option[B] =
    this.map(fab)((f: A => B) => fa.map(f).get)
}

val value1: Option[Int] = Some(4)
val value2: Option[Int] = Some(2)

value1 bind {a => a + 1}
value1 >>= {a => a + 1}

(value1 |@| value2) {_ + _}