package fpscala.chapter12

trait Applicative[F[_]] extends Functor[F]{
  def unit[A](a: => A):F[A]
  
  def ap[A,B](fab: F[A => B])(fa: F[A]):F[B] =
    this.map2(fab, fa)((f: A => B, a:A) => f(a))
    
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A,B) => C):F[C] = {
    val fab = this.map(fa)(f.curried)
    this.ap(fab)(fb)
  }
    
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc:F[C])(f: (A,B,C) => D): F[D] = {
    val fab = this.map(fa)(f.curried)
    this.ap(this.ap(fab)(fb))(fc)
  }
    
  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc:F[C], fd: F[D])(f: (A,B,C,D) => E): F[E] = {
    val fab = this.map(fa)(f.curried)
    this.ap(this.ap(this.ap(fab)(fb))(fc))(fd)
  }
    
  def map[A,B](fa: F[A])(f: A => B):F[B] = {
    ap(unit(f))(fa)
    //map2(fa, fa)((a:A, _) => f(a))
  }
  
  def sequence[A](fas: List[F[A]]): F[List[A]] = fas match {
    case List() => this.unit(List())
    case head :: tail => this.map2(head, this.sequence(tail)) { (a:A, fl: List[A]) =>
      a :: fl
    }
  }
  
  def replicateM[A](n: Int, fa: F[A]):F[List[A]] = {
    if (n == 1) 
      this.map2(fa, this.unit(List())) { (a:A, l:List[A]) =>
        a :: l
      }
    else if (n > 1)
      this.map2(fa, this.replicateM(n - 1, fa)) { (a:A, l:List[A]) =>
        a :: l
      }
    else unit(List())
  }
  
  def product[A,B](fa: F[A], fb: F[B]):F[(A,B)] = this.map2(fa, fb)((_,_))
  
  def traverse[A, B](as: List[A])(f: A => F[B]):F[List[B]] = 
    sequence(as map f)
}

object Implicits {
  implicit class applicativeSyntax[F[_]: Applicative, A](fa: F[A]) {
    def |@|[B, C](fb: F[B])(f: (A, B) => C): F[C] = implicitly[Applicative[F]].map2(fa, fb)(f)
    def ap[B](fab: F[A => B]):F[B] = implicitly[Applicative[F]].ap(fab)(fa)
    def map3[B,C,D](fb: F[B], fc:F[C])(f: (A,B,C) => D): F[D] = implicitly[Applicative[F]].map3(fa, fb, fc)(f)
    def map4[B,C,D,E](fb: F[B], fc:F[C], fd:F[D])(f: (A,B,C,D) => E): F[E] = implicitly[Applicative[F]].map4(fa, fb, fc, fd)(f)
    def replicateM(n: Int):F[List[A]] = implicitly[Applicative[F]].replicateM(n,fa)
    def product[B](fb: F[B]):F[(A,B)] = implicitly[Applicative[F]].product(fa, fb)
  }
  implicit class applicativeStyleSyntax[F[_]: Applicative, A, B](fab: F[A => B]) {
    def <*>(fa: F[A]):F[B] = implicitly[Applicative[F]].ap(fab)(fa)
  }
}
