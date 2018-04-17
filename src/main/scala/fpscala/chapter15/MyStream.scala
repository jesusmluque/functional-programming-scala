package fpscala.chapter15

/**
  * Created by jesus on 5/02/17.
  */
trait MyStream[+A] {
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

  def map[B](f: A => B): MyStream[B] =
    foldRight(MyStream.empty[B]) { (a, s) =>
      MyStream.cons(f(a), s)
    }

  def filter(f: A => Boolean): MyStream[A] =
    foldRight(MyStream.empty[A]) { (a, s) =>
      if (f(a)) {
        MyStream.cons(a, s)
      } else
        s
    }

  def flatMap[B](f: A => MyStream[B]): MyStream[B] =
    foldRight(MyStream.empty[B])(f(_).append(_))

  def append[B >: A](s: => MyStream[B]): MyStream[B] =
    foldRight(s)(MyStream.cons(_, _))

}

object MyStream {
  def cons[A](hd: => A, td: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = td

    Cons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = Empty

  def constant[A](a: A): MyStream[A] =
    cons(a, constant(a))

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def from(n: Int): MyStream[Int] =
    cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = f(z) match {
    case None => empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def fibs: MyStream[Int] =
    unfold((0, 1)){ s => {
        val value = s._1 + s._2
        Some(value, (s._2, value))
      }
    }

}