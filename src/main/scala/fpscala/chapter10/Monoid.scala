package fpscala.chapter10

/**
  * Created by jesus on 17/04/18.
  */
trait Monoid[A] {
  def op(a1:A, a2:A):A
  def zero: A
}
