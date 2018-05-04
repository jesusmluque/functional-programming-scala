import fpscala.chapter10.Monoid
import fpscala.chapter12.{Applicative, Traverse}
//A traverse instance for List
implicit val listTraverse = new Traverse[List] {
  override def traverse[G[_]:Applicative,A,B](as: List[A])(f: A => G[B]):G[List[B]] =
    implicitly[Applicative[G]].sequence(as map f)
}
//An applicative instance for Options
implicit val optionA2 = new Applicative[Option] {
  def unit[A](a: => A):Option[A] = Some(a)
  override def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A,B) => C): Option[C] =
    fb.map(fa.map(f.curried).get)
}

//Sequence execution without Opts.
implicitly[Traverse[List]].sequence(List(Option(1),Some(2)))

listTraverse.traverse(List(1,2,3,4)){a: Int =>
  Option(1 + a)

}
//Using Opts for Traverse: Sequence over List[Option] and
//traverse over List
import fpscala.chapter12.ImplicitsOts._
List(Option(1), Some(2)).sequence
List(1, 2).traverse(a => Option(a + 1))

//FoldMap with an instance of Monoid for Int
implicit val mono = new Monoid[Int] {
  def op(a:Int, b:Int) = a + b
  def zero:Int = 0
}
List(1,2,3,4,5).foldMap(_ + 1)