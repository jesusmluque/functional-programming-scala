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

  val p = (new { def f = "hola"}).f

  trait Const[A]
  type Id[A] = A

  val singletonList = new ~>[Id, List] {
    def apply[A](a: A):List[A] = List(a)
  }
  def pairList[A,B,C](l: Id ~> List, b:B, c: C):(List[B], List[C]) =
    (l(b), l(c))
  pairList(singletonList, 4, "hola")
  implicit val entero:Monoid[Int] = new Monoid[Int] {
    override def zero = 0
    override def op(a:Int, b:Int) = a + b
  }
  implicit val doble:Monoid[Double] = new Monoid[Double] {
    override def zero = 0.0
    override def op(a:Double, b:Double) = a + b
  }
  def getMonoid[A](implicit m: Monoid[A]) = m
  val monoidGeneral = new ~>[Id, Monoid] {

    def apply[A](a:A):Monoid[A] = getMonoid[A]

  }

  def dobleFold[A,B,C](m: Id ~> Monoid, l1: List[B], l2: List[C]):(B,C) = {
    (l1.foldLeft(m(l1.head).zero)(m(l1.head).op),
      l2.foldLeft(m(l2.head).zero)(m(l2.head).op))
  }
  //dobleFold(monoidGeneral, List(1,2,3,4,5), List(1.0,2.0,3.0,4.0,5.0))
}