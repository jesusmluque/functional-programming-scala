package fpscala.chapter12

/**
  * Created by jmontero on 11/05/2018.
  */
trait BiFunctor[F[_,_]] {
  def bimap[A,B,C,D](f: A => B, g: C => D)(fab: F[A,C]): F[B,D]
  def first[A,B,C,D](f: A => B)(fab: F[A,C]): F[B,C] = bimap(f, (a:C) => a)(fab)
  def second[A,B,C,D](f: C => D)(fab: F[A,C]): F[A,D] = bimap((a:A) => a, f)(fab)
}
object ImplicitsBiFunctor {
  implicit class biFunctorOpts[F[_, _]: BiFunctor, A,C](fab: F[A,C]) {
    def bimap[B,D](f: A => B, g: C => D): F[B,D] = implicitly[BiFunctor[F]].bimap(f,g)(fab)
  }
}