import fpscala.chapter13.Process
import fpscala.chapter15.MyStream
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

  s.map(_ + 1).filter(_ < 9).toList

  s.flatMap { a =>
    MyStream((1 to a): _*)
  }.filter(_ < 5).take(12).toList

  s.append(MyStream(10, 11, 12, 13)).toList

  MyStream.constant(4).take(5).toList

  MyStream.from(4).take(5).toList

  MyStream.fibs.take(10).toList

  val p = Process.lift((a: Int) => a + 1)

  val p2 = p.take(3)(Stream(1,2,3,4,5)).toList

  p.drop(2)(Stream(1,2,3,4,5)).toList

  p.takeWhile((i:Int) => i < 4)(Stream(1,2,3,4,5)).toList

  p.dropWhile((i:Int) => i < 4)(Stream(1,2,3,4,5)).toList

  p.counter(Stream(1,2,3,4,5)).toList
  Process.lift((a:String) => a).count2(Stream("a", "b", "c", "d", "e")).toList

  p.mean(Stream(1,2,3,4,5)).toList

  p.sum(Stream(1, 2, 3, 4, 5)).toList
  p.sum2(Stream(1, 2, 3, 4, 5)).toList

  val units = Stream.continually(())
  val ones = Process.lift((_:Unit) => 1)(units)
  ones.take(10).toList

  val pipe = Process.lift((a: Int) => a + 1) |> Process.lift((a: Int) => a + 1)
  pipe(Stream(1, 2, 3, 4, 5, 6)).toList

  

}