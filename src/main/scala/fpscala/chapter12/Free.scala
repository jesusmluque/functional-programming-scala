package fpscala.chapter12

import fpscala.chapter12.Translate.~>

/**
  * Created by jesus on 28/01/17.
  */
trait Free[F[_], A] { self =>
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(self, f)
  def map[B](f: A => B): Free[F, B] =
    flatMap(f andThen(Return(_)))

}
case class Return[F[_], A](a: A) extends Free[F,A]
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](sub: Free[F, A], f: A => Free[F, B]) extends Free[F,B]

object Free {
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = {
    new Monad[({type f[a] = Free[F, a]})#f] {
      override def unit[A](a: => A) = Return(a)
      override def map[A, B](fa: Free[F, A])(f: (A) => B): Free[F, B] = fa map f
      override def flatMap[A, B](fa: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] = fa flatMap f
    }
  }

  def run[F[_], G[_], A](a: Free[F,A])(t: F ~> G)(implicit G: Monad[G]): G[A] = step(a) match {
    case Return(a:A) => G.unit(a)
    case Suspend(fa) => t(fa)
    case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => run(f(a))(t))
    case _ => sys.error("Impossible, since `step` eliminates these cases")
  }

  def step[F[_], A](a: Free[F, A]): Free[F,A] = a match {
    case FlatMap(Return(x), f) => step(f(x))
    case FlatMap(FlatMap(sub, f1), f2) => step(sub.flatMap(y => f1(y) flatMap (z => f2(z))))
    case _ => a
  }
}
