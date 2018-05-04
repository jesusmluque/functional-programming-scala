package fpscala.chapter12

import fpscala.chapter10.Monoid

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  type Const[M,N] = M
  
  def monoidApplicative[M](M: Monoid[M]) = new Applicative[({ type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A):M = M.zero
      override def map2[A,B,C](m1: M, m2: M)(f: (A,B) => C): M = M.op(m1,m2)
    }

  def traverse[G[_],A,B](as: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(as)(f))
    
  def sequence[G[_],A](fga: F[G[A]])(implicit G: Applicative[G]):G[F[A]] = traverse(fga) { ga: G[A] =>
    ga
  }
  
  def map[A,B](fa: F[A])(f: A => B):F[B] = {
    type Id[Any] = Any
    val idApplicative = new Applicative[Id] {
      def unit[A](a: => A):Id[A] = a
      override def map2[A,B,C](fa: Id[A], fb: Id[B])(f: (A,B) => C): Id[C] =
        f(fa, fb)
    }
    traverse(fa){a: A =>
      idApplicative.unit(f(a))
    }(idApplicative)
  }
  
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]):B = {
    val monoidApp = monoidApplicative(mb)
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](as){a: A =>
      f(a)
    }(monoidApp)
    
  }
}
object ImplicitsOts {

  implicit class TraverseOpts[F[_]: Traverse, G[_]: Applicative, A](t: F[G[A]]) {
    def sequence:G[F[A]] = implicitly[Traverse[F]].sequence(t)
  }
  implicit class SequenceOpts[F[_]: Traverse, G[_]: Applicative, A](t: F[A]) {
    def traverse[B](f: A => G[B]): G[F[B]] = implicitly[Traverse[F]].traverse(t)(f)
    def foldMap[B](f: A => B)(implicit mb: Monoid[B]):B = implicitly[Traverse[F]].foldMap(t)(f)(mb)
  }

}