import org.scalatest._

/**
 * User: ken
 * Date: 2014-01-05 2:06 PM
 */
class ListProblemsSpec extends WordSpec with Matchers with ListProblems {
  "Calling last on a list" should {
    "give us the last element of a list" in {
      val l = List(1,2,3,4,5)

      assertResult(5) { last(l) }

    }

  }

  "calling findNthElement on a list" should {
    "give us the nth element of the list" in {
      val l = List(1,2,3,4,5)
      assertResult(3){ findNthElement(2, l) }
    }
  }

  "calling length on a list " should {
    "give us the lenght of the list" in {
      val l = List(1,2,3,4,5)
      assertResult(5) { length(l) }


    }
  }

  "calling reverseList on a list " should {
    "give us the same list in reverse order" in {
      val l = List(1,2,3,4,5)

      println(reverseList(l))

    }
  }

  " calling duplicate on a list " should {
    "give us a duplicated list" in {
      val l = List(1,2,3,4)


      println(duplicate(l))
    }
  }


  "calling encodedModified on a list " should {
    "return a list where only elements with duplicates are transferred as (N, E) terms" in {
      val input = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

      assertResult(List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))) { encodedModified(input) }
    }
  }


  "calling drop on a list" should {
    "drop every Nth element from the list" in {
      val input =  List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

      assertResult(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)){ drop(3, input) }
    }
  }
}
