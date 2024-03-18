package u03

enum Person:
  case Student(name: String, year: Int)
  case Teacher(name: String, course: String)

object Person:
  def name(p: Person): String = p match
    case Student(n, _) => n
    case Teacher(n, _) => n

  def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _ => false

object Task3:
  import Person.*
  import Sequences.*
  import Sequence.*

  def coursesOfTeachers(l: Sequence[Person]): Sequence[String] =
    flatMap(l)(p => p match
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    )
