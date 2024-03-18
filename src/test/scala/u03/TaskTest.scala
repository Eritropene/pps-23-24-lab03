package u03

import Optionals.Optional.*
import org.junit.*
import org.junit.Assert.*
import Person.*
import Sequences.*
import Sequence.*
class TaskTest:

  import Task3.*

  val people: Sequence[Person] = Cons(Student("Aldo", 2020),
                                  Cons(Teacher("Bob", "LCMC"),
                                    Cons(Teacher("Carl", "Automazione"),
                                      Cons(Teacher("Dan", "Musica Medievale"), Nil()))))
  @Test def testCoursesOfTeachers() =
    assertEquals(Cons("LCMC", Cons("Automazione", Cons("Musica Medievale", Nil()))), coursesOfTeachers(people))

