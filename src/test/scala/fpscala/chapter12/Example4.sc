import fpscala.chapter12._

object Example4 {
  println("hola")

  val optionA = new Applicative[Option] {
    def unit[A](a:A):Option[A] = Some(a)
    override def map[A,B](fa: Option[A])(f: A => B):Option[B] = fa map f
    override def apply[A,B](fab: Option[A => B])(fa: Option[A]):Option[B] =
      fab map ((f: A => B) => fa.map(f).get)
  }

  optionA.map2(Some(3),Some(5))(_ + _)
  optionA.apply(Some((_:String) + " hola"))(Some("Pedro"))

  implicit val optionA2 = new Applicative[Option] {
    def unit[A](a:A):Option[A] = Some(a)
    override def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A,B) => C): Option[C] =
      fb.map(fa.map(f.curried).get)
  }


  optionA2.map2(Some(3),Some(5))(_ + _)
  optionA2.apply(Some((_:String) + " hola"))(Some("Pedro"))
  optionA2.map3(Some(4), Some(5), Some(6))(_ + _ + _)

  optionA2.map4(Some(4), Some(5), Some(6), Some(7))(_ + _ + _ + _)

  optionA2.sequence(List(Some(1), Some(2), Some(3)))

  optionA2.replicateM(6, Some(1))

  optionA2.product(Some(2), Some(4))

  val optionM = new Monad[Option] {
    def unit[A](a: A):Option[A] = Some(a)
    override def join[A](ffa: Option[Option[A]]):Option[A] = ffa.get
    override def map[A,B](fa: Option[A])(f: A => B) = fa map f
    override def apply[A,B](fab: Option[A => B])(fa: Option[A]):Option[B] =
      this.map(fab)((f: A => B) => fa.map(f).get)
  }

  optionM.compose((a:Int) => Some(a + 1), (a:Int) => Some(a + 2))(4)


  val streamApplicative = new Applicative[Stream] {
    override def unit[A](a: A):Stream[A] = Stream.continually(a)
    override def map[A,B](fa: Stream[A])(f: A => B) = fa map f
    override def map2[A,B,C](fa: Stream[A], fb: Stream[B])(f: (A,B) => C):Stream[C] =
      map(fa.zip(fb))(f.tupled)
  }

  streamApplicative.map2(streamApplicative.unit(1), streamApplicative.unit(2))(_ + _)

  streamApplicative.sequence(List(streamApplicative.unit(1), streamApplicative.unit(2)))

  val listTraverse = new Traverse[List] {
    override def traverse[G[_]:Applicative,A,B](as: List[A])(f: A => G[B]):G[List[B]] =
      implicitly[Applicative[G]].sequence(as map f)
  }

  listTraverse.sequence(List(Option(1),Some(2)))

  /*listTraverse.traverse(List(1,2,3,4)){a: Int =>
    Option(1 + a)

  }*/

  val mono = new Monoid[Int] {
    def op(a:Int, b:Int) = a + b
    def zero:Int = 0
  }

  listTraverse.foldMap(List(1,2,3,4,5)) {a:Int =>
    a
  }(mono)

}