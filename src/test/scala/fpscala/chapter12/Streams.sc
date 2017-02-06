import fpscala.chapter12._
object Streams {
  val s = MyStream(1,2,3,4,5,6,7,8,9)



  val s2 = s.take(4)

  s2.toList

  s.drop(4).toList

  s.foldRight(0)(_ + _)

  s.exists(_ == 4)
  s.exists(_ == 0)

  s.forAll(_ < 10)

  s.forAll(_ > 10)

  s.takeWhile(_ < 5).toList



}