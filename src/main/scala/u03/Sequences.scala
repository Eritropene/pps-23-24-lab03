package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = 
      flatMap(l)(a => Cons(mapper(a), Nil()))

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] =
      flatMap(l1)(a => a match
        case i if pred(i) => Cons(i, Nil())
        case _            => Nil()
      )

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Nil(), _)                   => Nil()
      case (_, Nil())                   => Nil()
      case (Cons(fh, ft), Cons(sh, st)) => Cons((fh, sh), zip(ft, st))

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = (l, n) match
      case (_, i) if i <= 0 => Nil()
      case (Nil(), _)       => Nil()
      case (Cons(h, t), i)  => Cons(h, take(t)(i-1))
    
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Nil()      => l2
      case Cons(h, t) => Cons(h, concat(t, l2))
    
    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Nil()      => Nil()
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))

    def min(l: Sequence[Int]): Optional[Int] = l match
      case Nil()      => Optional.Empty()
      case Cons(h, t) =>
        min(t) match
          case Optional.Empty()           => Optional.Just(h)
          case Optional.Just(v) if h < v  => Optional.Just(h)
          case Optional.Just(v)           => Optional.Just(v)
    
@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
