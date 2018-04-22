import scalaz.ioeffect._


val hola = IO.point("hola")

val inter = new RTS {}

inter.unsafePerformIO(hola)

val makeActor =
  for {
    counter <- IORef(0)
    actor = (n: Int) => counter.modify(_ + n)
  } yield actor

val a1 = for {
  v <- makeActor
} yield v(20)


inter.unsafePerformIO(inter.unsafePerformIO(a1))

