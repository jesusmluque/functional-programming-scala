import fpscala.chapter12.Applicative
import fpscala.chapter12.Implicits._

val optionA = new Applicative[Option] {
  override def unit[A](a: => A):Option[A] = Some(a)
  override def map[A,B](fa: Option[A])(f: A => B):Option[B] = fa map f
  override def ap[A,B](fab: Option[A => B])(fa: Option[A]):Option[B] =
    fab map ((f: A => B) => fa.map(f).get)
}

implicit val optionA2 = new Applicative[Option] {
  def unit[A](a: => A):Option[A] = Some(a)
  override def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A,B) => C): Option[C] =
    fb.map(fa.map(f.curried).get)
}

optionA2.map2(Some(3),Some(5))(_ + _)
(Option(3) |@| Some(5)) {_ + _}

optionA2.ap(Some((_:String) + " hola"))(Some("Pedro"))
Option("Pedro").ap(Some[String => String](_ + " hola"))

optionA2.map3(Some(4), Some(5), Some(6))(_ + _ + _)
Option(4).map3(Some(5), Some(6)){_ + _ + _}

optionA2.map4(Some(4), Some(5), Some(6), Some(7))(_ + _ + _ + _)
Option(4).map4(Some(5), Some(6), Some(7)){_ + _ + _ + _}

optionA2.replicateM(6, Some(1))
Option(1).replicateM(6)

optionA2.product(Some(2), Some(4))
Option(2).product(Some(4))

//Applicative instance for Streams
implicit val streamApplicative = new Applicative[Stream] {
  override def unit[A](a: => A):Stream[A] = Stream.continually(a)
  override def map[A,B](fa: Stream[A])(f: A => B) = fa map f
  override def map2[A,B,C](fa: Stream[A], fb: Stream[B])(f: (A,B) => C):Stream[C] =
    map(fa.zip(fb))(f.tupled)

}

streamApplicative.map2(streamApplicative.unit(1), streamApplicative.unit(2))(_ + _)

streamApplicative.sequence(List(streamApplicative.unit(1), streamApplicative.unit(2)))


(Stream(1, 2) |@| Stream(1, 2)) {_ + _} toList