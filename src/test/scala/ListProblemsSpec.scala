import org.scalatest._
import scala.util.Random

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

    }
  }

  " calling duplicate on a list " should {
    "give us a duplicated list" in {
      val l = List(1,2,3,4)
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

  "calling split on a list " should {
    "split a list into two lists" in {
      val input = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

      assertResult((List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))) {
        split(3, input)
      }

    }
  }

  "calling insertAt on a list " should {
    "insert an element into the list at the specified position" in {

      assertResult( List('a, 'new, 'b, 'c, 'd) ) {
        insertAt('new, 1, List('a, 'b, 'c, 'd))
      }
    }
  }

  "calling range  " should {
    "give you a list that contains all the integers between the start and end specified" in {

      assertResult( List(4, 5, 6, 7, 8, 9)) {
        range(4, 9)
      }
    }

  }

  "calling randomSelect on a list " should {
    "give you a list of a specified size, containing randomly selected elements from the list" in {

      val input = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
      val randomSize = Random.nextInt( input.size )

      val result = randomSelect(randomSize, input)

      //how do we even test this properly?
      assert(result.size == randomSize, "Failed when random size is: " + randomSize)
    }
  }

}
