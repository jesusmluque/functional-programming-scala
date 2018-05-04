package fpscala.chapter10

/**
  * Created by jesus on 17/04/18.
  */
trait Monoid[A] {
  def op(a1:A, a2:A):A
  def zero: A
}

object Implicits {
  implicit class monoidOps[A: Monoid](a1: A) {
    def op(a2: A) = implicitly[Monoid[A]].op(a1, a2)
    def zero = implicitly[Monoid[A]].zero
  }
}
