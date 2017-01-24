package fpscala.chapter12

trait Foldable[F[_]] {
  //def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B):B
  //def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B):B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]):B
}