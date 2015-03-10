package learn

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz._, Scalaz._

/**
 * @author alari
 * @since 3/24/14
 */
object Main extends App{
  println("hi !")

  println(10.some | 5)

  assert(1 === 1)

  assert(1.some =/= 2.some)

  println(1 ?|? 2)
  println(1 lte 2)

  sealed trait OrdTest
  case object OT1 extends OrdTest
  case object OT2 extends OrdTest
  implicit object OrdTestOrder extends Order[OrdTest] {
    override def order(x: OrdTest, y: OrdTest): Ordering = (x, y) match {
      case (a, b) if a eq b => Ordering.EQ
      case (OT1, _) => Ordering.LT
      case _ => Ordering.GT
    }
  }

  implicit object shower extends Show[OrdTest] {
    override def shows(ot: OrdTest) = ot.toString
  }

  println((OT1: OrdTest) ?|? OT2)
  println((OT2: OrdTest) ?|? OT1)

  println((OT1: OrdTest).show)

  println('a' to 'e', 'a' |-> 'd')
  println('a' |==> (2, 'f'))

  println(implicitly[Enum[Char]].max)

  def f = (i: Int) => i.toString
  def g = (s: String) => s.charAt(0)

  def gf = g compose f

  def gf2 = f andThen g

  assert(gf(56) == '5')
  assert(gf(90) == gf2(900))

  // Applicative is something with a pointer to get into category
  assert(1.point[List] == List(1))

  // point brings to a category, strength makes turples with one constant
  println(1.point[Option] map {(_:Int)^2} strengthR "y")

  // apply to last item in turple (Functor)
  println( (1, 2, 3) map {_ * 5} )

  println(Functor[List].lift((_: Int) + 2).apply(1.point[List]) strengthR "functor lifts something inside a category")

  // function composition
  println( 3.some <*> ( ((x:Int) => x*x*x) map ((_: Int) / 2) ).some )

  // Apply typeclass
  println(9.some <*> {(_: Int) + 3}.some)
  println(1.point[List] <*> {(_: Int) + 3}.point[List])
  println((3.some |@| 5.some) {_ + _} strengthL "it lets unwrap and apply")
  println((1.point[Vector] |@| 2.point[Vector]){_+_} strengthR "and also available for lists and vectors")

  val liftedApply =  Apply[Option].lift2((_: Int) :: (_: List[Int]))
  assert(liftedApply(3.some, List(4).some) === List(3, 4).some )
  val la2 = Apply[List].lift2((_: String) :: (_: Option[String]).point[List])
  println(la2("h".point[List], "fuck".point[Option].point[List]))

  // sequence of applicatives to applicative of sequence of values
  import scala.language.higherKinds
  def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
    case Nil     => (Nil: List[A]).point[F]
    case x :: xs => (x |@| sequenceA(xs)) {_ :: _}
  }

  // tagged types
//  sealed trait KiloGram
//  def KiloGram[A](a: A): A @@ KiloGram = Tag[A, KiloGram](a)
//  val mass = KiloGram(20.0)
//  println(mass)
//  println(2 * Tag.unwrap(mass))
//  sealed trait JoulePerKiloGram
//  def JoulePerKiloGram[A](a: A): A @@ JoulePerKiloGram = Tag[A, JoulePerKiloGram](a)
//  def energyR(m: Double @@ KiloGram): Double @@ JoulePerKiloGram = JoulePerKiloGram(299792458.0 * 299792458.0 * Tag.unwrap(m))
//  println(energyR(mass))

  // monoid for multiplication of numbers
  assert((Tags.Multiplication(10) |+| Monoid[Int @@ Tags.Multiplication].zero |+| Tags.Multiplication(10)) === Tags.Multiplication(100))


  // ordering is also a monoid
  def lengthCompare(lhs: String, rhs: String): Ordering = (lhs.length ?|? rhs.length) |+| (lhs ?|? rhs)
  println(lengthCompare("scalaz", "appelsin"))
  println(lengthCompare("zeppelin", "appelsin"))

  // functor laws
  List(1, 2, 3) map {identity} assert_=== List(1, 2, 3)
  (List(1, 2, 3) map {{(_: Int) * 3} map {(_: Int) + 1}}) assert_=== (List(1, 2, 3) map {(_: Int) * 3} map {(_: Int) + 1})

  // option monoid
  (none: Option[String]) |+| "andy".some

  (Tags.First('a'.some) |+| Tags.First('b'.some)) == 'a'.some
  (Tags.Last('a'.some) |+| Tags.Last('b'.some)) == 'b'.some

  // foldable
  assert((List(1, 2, 3) foldMap {identity}) == 6)
  // There is no default Monoid for booleans, but there are Monoids
  // for booleans tagged with either the Conjunction or Disjunction
  // tag.
  assert((Tags.Conjunction(true) |+| Tags.Conjunction(true)) === Tags.Conjunction(true))
  assert((Tags.Conjunction(true) |+| Tags.Conjunction(false)) === Tags.Conjunction(false))
  assert((Tags.Conjunction(false) |+| Tags.Conjunction(false)) === Tags.Conjunction(false))
  assert(Monoid[Boolean @@ Tags.Conjunction].zero === Tags.Conjunction(true))

  assert((Tags.Disjunction(true) |+| Tags.Disjunction(true)) === Tags.Disjunction(true))
  assert((Tags.Disjunction(true) |+| Tags.Disjunction(false)) === Tags.Disjunction(true))
  assert((Tags.Disjunction(false) |+| Tags.Disjunction(false)) === Tags.Disjunction(false))
  assert(Monoid[Boolean @@ Tags.Disjunction].zero === Tags.Disjunction(false))

  val ff = for {
    x <- 1 |-> 50 if x.shows contains '7'
  } yield x
  val mpf = (1 |-> 50) filter { x => x.shows contains '7' }
  assert(ff === mpf)

  val mpl = List(1, 2, 3) <+> List(4, 5, 6)
  assert(mpl === (1 |-> 6))

  println(List(1, 2, 3) |+| List(4, 5, 6))

  // Writer
  val three = 3.set("three")
  val four = 4.set("four")
  println(three.written |+| four.written)

  // Reader
  val addStuff: Int => Int = for {
    a <- (_: Int) * 2
    b <- (_: Int) + 10
  } yield a + b
  println(3 |> addStuff)

  // Functional State machine
  type Stack = List[Int]
  val pop = State[Stack, Int] {
    case x :: xs => (xs, x)
  }
  def push(a: Int) = State[Stack, Unit] {
    case xs => (a :: xs, ())
  }
  def stackManip: State[Stack, Int] = for {
    _ <- push(3)
    _ <- pop
    b <- pop
  } yield b
  println(stackManip(List(5, 8, 2, 1)))

  // Trying lenses
  case class ItemImage(src: String, width: Int)
  case class Item(title: String, image: ItemImage)
  val titem = Item("test item", ItemImage("test src", 55))
  val itemTitle = Lens.lensu[Item,String](
    (o, v) => o.copy(title = v),
    _.title
  )
  println("title = " + itemTitle.get(titem))
  println("title' = " + itemTitle.get(itemTitle.set(titem, "new changed title")) )

  val itemImage = Lens.lensu[Item,ItemImage] (
    (o, v) => o.copy(image = v),
    _.image
    )
  println("image = " + itemImage.get(titem))

  val imageSrc = Lens.lensu[ItemImage,String] (
    {(o, v) => o.copy(src = v)},
    _.src
  )

  val itemImageSrc = imageSrc compose itemImage
  itemImage >=> imageSrc

  println("item image src = " + itemImageSrc.get(titem))
  println("changed src = " + itemImageSrc.set(titem, "new src"))

  // Either \/
  1.right[String] assert_=== \/-(1)
  15.left[Double] assert_=== -\/(15)
  (15.right[String] | 16) assert_=== 15

  val oneOf = for {
    e1 <- "event 1 ok".right
    e2 <- "event 2 failed!".left[String]
    e3 <- "event 3 failed!".left[String]
  } yield e1 |+| e2 |+| e3
  oneOf assert_=== "event 2 failed!".left[String]

  "boom".left[Int] flatMap { x: Int => (x + 1).right[String] }
  ("event 1 failed!".left ||| "retry event 1 ok".right) assert_=== \/-("retry event 1 ok")

  // Validation
  1.wrapNel
  val eInval = ("event 1 ok".success[String] |@| "event 2 failed!".failure[String] |@| "event 3 failed!".failure[String]) {_ + _ + _}
  eInval.isFailure assert_=== true
  val eNelval = ("event 1 ok".successNel[String] |@| "event 2 failed!".failureNel[String] |@| "event 3 failed!".failureNel[String]) {_ + _ + _}
  eNelval match {
    case Success(a) =>
      println(a)
    case Failure(b) =>
      println(b)
  }

  // Monadic ops
  (Some(9.some): Option[Option[Int]]).join assert_=== 9.some
  List(List(1, 2, 3), List(4, 5, 6)).join assert_=== List(1, 2, 3, 4, 5, 6)

  def binSmalls(acc: Int, x: Int): Option[Int] = {
    if (x > 9) none: Option[Int]
    else (acc + x).some
  }

  List(2, 8, 3, 1).foldLeftM(0)(binSmalls) assert_=== 14.some

  val r = Reader((_: Int) + 1)
  val ro = for {
    o <- r >=> Reader(_ - 2)
  } yield o

  println("Kleisli produces "+ro(0))





  def testScalaz: String = {
    import scalaz._
    import Scalaz._

    def addString(s: String) = Kleisli[Option, Int, String] {
      a =>
        (a + s).some
    }

    def markPong = Kleisli[Option, String, String] {
      a => (a + " - pong").some
    }

    def pongPong = addString("-p-o-n-g") andThen markPong

    type V[T] = \/[String,T]

    def otherEffect = Kleisli[V, String, String] {
      s =>
        if(s == "ok") \/-("right")
        else -\/("left")
    }

    case class Domain[T](v: String)

    sealed trait TState
    sealed trait TOddState extends TState
    sealed trait TEvenState extends TState
    trait T_0 extends TEvenState
    trait T_1 extends TOddState
    trait T_2 extends TEvenState
    trait T_3 extends TOddState

    type TOddDomain = Domain[_ <: TOddState]
    type TEvenDomain = Domain[_ <: TEvenState]
    type T1Domain = Domain[T_1]
    type T0Domain = Domain[T_0]

    def printOdd = Kleisli[V, TOddDomain, String]{
      t => \/-("odd "+t.v)
    }

    def printEven = Kleisli[V, TEvenDomain, String]{
      t => \/-("even "+t.v)
    }

    def toOdd = Kleisli[V, T0Domain, TOddDomain]{
      t => t.copy[T_1]().right.map(identity[TOddDomain])
    }

    printOdd.run( Domain[T_1]("1") )
    printEven.run( Domain[T_2]("1") )

    def fk = Kleisli[Future, String, String]{
      s => Future(s)
    }

    def ak: String => Int = _.reverseIterator.mkString.toInt

    def fak = fk map ak

    def fkfak = fk map fak

    fak

    fkfak

    pongPong.run(1).getOrElse("ping")
  }


  {

    def curry[A, B, C](f: (A, B) => C): A => B => C = a => f(a, _)
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = f(_)(_)
    def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
    @tailrec
    def drop[T](l: List[T], n: Int): List[T] = n match {
      case 0 => l
      case _ => drop(l.tail, n - 1)
    }
    def length[T](l: List[T]): Int = l.foldRight(0) {
      case (_, n) => n + 1
    }
    def append[T](l: List[T], e: T): List[T] = l.foldRight(List(e)) {
      case (x, xs) => x :: xs
    }
    def flatten[T](l: List[List[T]]): List[T] = l.foldLeft(Nil: List[T])(_ ++ _)
    println("append " + append(List(1, 2), 3))
    println(List(1, 2, 3).foldLeft(Nil: List[Int]) {
      case (xs, x) => x :: xs
    })
    def filter[A](l: List[A])(f: A => Boolean) = l.flatMap {
      case m if f(m) => m :: Nil
      case m =>
        Nil
    }
    @tailrec
    def hasPrefix[A](sup: List[A], pref: List[A]): Boolean = if (pref == Nil) true
    else sup match {
      case `pref` => true
      case x :: xs =>
        pref match {
          case `x` :: ys =>
            hasPrefix(xs, ys)
          case _ => false
        }
      case _ => false
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
      if (sub == Nil) true
      else sup match {
        case `sub` => true
        case _ if sub.length >= sup.length =>
          false
        case _ if hasPrefix(sup, sub) => true
        case _ :: xs => hasSubsequence(xs, sub)
      }


    val lst = List(1, 2, 3, 4)
    println("pref 1,2 in l = " + hasPrefix(lst, List(1, 2)))
    println("1,2 in l = " + hasSubsequence(lst, List(1, 2)))
    println("2,3 in l = " + hasSubsequence(lst, List(2, 3)))
    println("2,3,4 in l = " + hasSubsequence(lst, List(2, 3, 4)))
    println("4 in l = " + hasSubsequence(lst, List(4)))
    println("2,4 in l = " + hasSubsequence(lst, List(2, 4)))
    println("5 in l = " + hasSubsequence(lst, List(5)))



    println("l = " + length(1 :: 2 :: Nil))

  }

  {
    sealed trait Tree[A]
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    case class Leaf[A](value: A) extends Tree[A]

    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
    def top[A](tree: Tree[A])(f: (A, A) => A): A = tree match {
      case Leaf(v) => v
      case Branch(l, r) => f(top(l)(f), top(r)(f))
    }
    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
    def sizeFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)
    def topFold[A](t: Tree[A], f: (A, A) => A): A = fold(t)(identity)(f)
    def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
  }

  {
    def liftOpt[A,B](f: A => B): Option[A] => Option[B] = _ map f
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B)=>C): Option[C] = a.flatMap(v => b.map(f(v, _)))

    def seqOpt[A](l: List[Option[A]]): Option[List[A]] = traverse(l)(identity)

    def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => Some(Nil)
      case x :: xs =>
        f(x).flatMap(v => traverse(xs)(f).map(v :: _))
    }

    def traverseE[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] = l match {
      case Nil => Right(Nil)
      case x :: xs =>
        f(x).right.flatMap(v => traverseE(xs)(f).right.map(v :: _))
    }
  }


}
