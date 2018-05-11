import fpscala.chapter10.Monoid
import fpscala.chapter11.Monad
import fpscala.chapter12._
import fpscala.chapter12.ImplicitsBiFunctor._

object Example4 {


  val optionM = new Monad[Option] {
    def unit[A](a: => A):Option[A] = Some(a)
    override def join[A](ffa: Option[Option[A]]):Option[A] = ffa.get
    override def map[A,B](fa: Option[A])(f: A => B) = fa map f
    override def ap[A,B](fab: Option[A => B])(fa: Option[A]):Option[B] =
      this.map(fab)((f: A => B) => fa.map(f).get)
  }

  optionM.compose((a:Int) => Some(a + 1), (a:Int) => Some(a + 2))(4)

  implicit val tupleBi = new BiFunctor[Tuple2] {
    override def bimap[A, B, C, D](f: (A) => B, g: (C) => D)(fab: (A, C)): (B, D) = {
      (f(fab._1), g(fab._2))
    }
  }

  val r = (1, 2) bimap (a => a + 1, b => b + 2)

}