package fpscala.chapter12

/**
  * Created by jesus on 5/02/17.
  */
sealed trait MyStream[+A] {
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): MyStream[A] = this match {
    case Empty => Empty
    case _ if n == 0 => Empty
    case Cons(h, t) => Cons(() => h(), () => t().take(n - 1))
  }

  def drop(n: Int): MyStream[A] = {
    def streamDrop(s: MyStream[A], num: Int): MyStream[A] = s match {
      case Empty => Empty
      case _ if n > 0 => Empty
      case Cons(h, t) => Cons(() => h(), () => streamDrop(t(), num - 1))
    }
    streamDrop(this, n)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    this.foldRight(false) {(a, z) =>
      z || p(a)
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, z) =>
      z && p(a)
    }

  def takeWhile(p: A => Boolean): MyStream[A] =
    foldRight(MyStream.empty[A]) { (a, s) =>
      if (p(a))
        MyStream.cons(a, s)
      else
        s
    }

}
case object Empty extends MyStream[Nothing]
case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {
  def cons[A](hd: => A, td: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = td

    Cons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


}
