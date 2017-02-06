import fpscala.chapter12.Translate.~>
import fpscala.chapter12._


object IOExamples {

  def runTrampoline[A](f: Free[Function0, A]): A = f match {
    case Return(a:A) => a
    case Suspend(f) => f()
    case FlatMap(sub, f) => sub match {
      case Return(a: A) => runTrampoline(f(a))
      case Suspend(resume) => runTrampoline(f(resume()))
      case FlatMap(x:Free[Function0, A],y) => runTrampoline(x flatMap (a => y(a) flatMap(f)))
    }
  }
  def printLine(msg: String):Free[Function0, Unit] =
    Suspend(() => println(msg))

  case class Player(name: String, score: Int)

  def winner(pl1:Player, pl2:Player): Option[Player] =
    if (pl1.score > pl2.score)
      Some(pl1)
    else if (pl2.score > pl1.score)
      Some(pl2)
    else None

  def contest(pl1: Player, pl2: Player): Free[Function0, Unit] =
    winner(pl1, pl2) match {
      case Some(pl) => printLine("El ganador es " + pl.name)
      case None => printLine("Empatados")
    }

  val res = contest(Player("Manolo", 10), Player("Jesus", 11))

  runTrampoline(Free.freeMonad[Function0].map(res)((_) => println("hola")))

  val prueba = for {
    _ <- Console.printLn("hola esta es la primera")
    _ <- Console.printLn("esta es la segunda")
  } yield ()

  val consoleToFunction = new (Console ~> Function0) {
    override def apply[A](f: Console[A]) = f.toThunk
  }
  implicit val function0Monad = new Monad[Function0] {
    override def unit[A](a: => A) = () => a
    override def flatMap[A, B](fa: () => A)(f: (A) => () => B): () => B =
      () => f(fa())()
  }
  Free.run[Console, Function0, Unit](prueba)(consoleToFunction).apply()
}