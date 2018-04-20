package fpscala.chapter13

/**
  * Created by jesus on 18/02/17.
  */
trait Process[I, O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(r) => s match {
      case h #:: t => r(Some(h))(t)
      case xs => r(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(r) => Await {
        case None =>  r(None)
        case i => go(r(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }

  def filter(p: I => Boolean): Process[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit[I, I](i)
    case _ => Halt()
  }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] = Await {
      case Some(i) => Emit(acc + i, go(acc + i))
      case None => Halt()
    }
    go(0.0)
  }

  def take[I](n: Int): Process[I, I] = {
    def go(num: Int):Process[I, I] = Await {
      case Some(_) if n == num => Halt()
      case None => Halt()
      case Some(i) => Emit(i, go(num + 1))
    }
    go(0)
  }

  def drop[I](n: Int): Process[I, I] = {
    def go(num: Int): Process[I,I] = Await {
      case Some(_) if num < n => go(num + 1)
      case None => Halt()
      case Some(i) => Emit(i, go(num + 1))
    }
    go(0)
  }

  def takeWhile[I](f: I => Boolean): Process[I, I] = {
    def go(f: I => Boolean):Process[I, I] = Await {
      case Some(i) if f(i) => Emit(i, go(f))
      case _ => Halt()
    }
    go(f)
  }

  def dropWhile[I](f: I => Boolean): Process[I, I] = {
    def go(f: I => Boolean):Process[I, I] = Await {
      case Some(i) if f(i) => go(f)
      case None => Halt()
      case Some(i) => Emit(i, go(f))
    }
    go(f)
  }

  def counter: Process[I, Int] = {
    def go(index: Int): Process[I, Int] = Await {
      case Some(_) => Emit(index, go(index + 1))
      case None => Halt()
    }
    go(0)
  }

  def mean: Process[Double, Double] = {
    def go(num: Double, acc: Double): Process[Double, Double] = Await {
      case Some(i) => Emit((acc + i)/num, go(num + 1, acc + i))
      case None => Halt()
    }
    go(1.0, 0.0)
  }

  def loop[S, I, O](z: S)(f: (S, I) => (S, O)): Process[I, O] = {
    def go(s: S, f: (S, I) => (S, O)): Process[I, O] = Await {
      case Some(i) => f(s, i) match {
        case (s2, i2) => Emit(i2, go(s2, f))
      }
      case None => Halt()
    }
    go(z, f)
  }

  def sum2: Process[Double, Double] = loop(0.0){ (s, i) =>
    (s + i, s + i)
  }

  def count2: Process[I, Int] = loop(0) { (s, _) =>
    (s + 1, s + 1)
  }

  def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
    case Halt() => Halt()
    case Emit(a, t) => Emit(a, this |> t)
    case Await(r) => this match {
      case Halt() => Halt() |> r(None)
      case Emit(a, t) => t |> r(Some(a))
      case Await(r2) => Await(i => r2(i) |> p2)
    }
  }

  def ++(p2: Process[I, O]):Process[I, O] = this match {
    case Halt() => p2
    case Emit(a, t) =>  Emit(a, this ++ t)
    case Await(r) => Await(r andThen (_ ++ p2))
  }

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

  def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = this match {
    case Halt() => Halt()
    case Emit(a, t) => f(a) ++ t.flatMap(f)
    case Await(r) => Await(r andThen (_ flatMap f))
  }
}

object Process {

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None => Halt()
    }
  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat
}