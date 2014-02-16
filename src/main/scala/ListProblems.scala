import java.util.NoSuchElementException
import scala.NoSuchElementException

/**
 * User: ken
 * Date: 2014-01-05 2:04 PM
 */
trait ListProblems {

  /*
    Find the last element of a list.
    Example:
    scala> last(List(1, 1, 2, 3, 5, 8))
    res0: Int = 8
   */
  def last[T]( input: List[T] ): T =  {
    input match {
      case h :: Nil => h
      case _ :: tail => last(tail)
      case _ => throw new NoSuchElementException
    }
  }

  /*
    Find the last but one element of a list.
    Example:
    scala> penultimate(List(1, 1, 2, 3, 5, 8))
    res0: Int = 5
   */
  def secondLast[T]( input: List[T]): T = {
    input match {
      case h :: s :: Nil => h
      case _ :: tail => secondLast(tail)
      case _ => throw new NoSuchElementException
    }
  }

  /*
    Find the Kth element of a list.
    By convention, the first element in the list is element 0.
    Example:

    scala> nth(2, List(1, 1, 2, 3, 5, 8))
    res0: Int = 2
   */
  def findNthElement[T](position: Int, input: List[T]): T = {

    //note: this may not be the most efficient. Finding the length may take longer than finding the Nth element
    require(position < input.length )

    if ( position == 0 ) input.head
    else findNthElement(position - 1, input.tail)
  }

  /*
    Find the number of elements of a list.
    Example:
    scala> length(List(1, 1, 2, 3, 5, 8))
    res0: Int = 6
   */
  def length[T](input: List[T]): Int = {
    input.foldLeft(0)( (acc, _) => acc + 1 )
  }

  /*
    Reverse a list.

    Example:
    scala> reverse(List(1, 1, 2, 3, 5, 8))
    res0: List[Int] = List(8, 5, 3, 2, 1, 1)
   */
  def reverseList[T](input: List[T]): List[T] = {
//    input.foldRight( List[T]() )((acc, element) => element :+ acc)

    //the north wind comes from the north
    input.foldLeft( List[T]() )( (acc, head) => head +: acc )
  }


  /*
    p07
    Flatten a nested list
    Example:
    scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    res0: List[Any] = List(1, 1, 2, 3, 5, 8)
   */
  def flatten[Any](lists: List[Any]): List[Any] = {

    lists match {
      case x :: Nil => ???
    }

  }


  /*
    p08
    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
    Example:

    scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
   */
  def compress = ???

  // P09 (**) Pack consecutive duplicates of list elements into sublists.
  //     If a list contains repeated elements they should be placed in separate
  //     sublists.
  //
  //     Example:
  //     scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //     res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  // P10 (*) Run-length encoding of a list.
  //     Use the result of problem P09 to implement the so-called run-length
  //     encoding data compression method.  Consecutive duplicates of elements are
  //     encoded as tuples (N, E) where N is the number of duplicates of the
  //     element E.
  //
  //     Example:
  //     scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
    def encode[A](ls: List[A]): List[(Int, A)] = pack(ls) map { e => (e.length, e.head) }

  /**
    p11. Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
    Example:

    scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
 */
  def encodedModified[T](ls: List[T]): List[Any] = encode(ls) map {
      case (1,letter) => letter
      case x @ _ => x
  }

  /*
    P12 (**) Decode a run-length encoded list.
    Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
    Example:

    scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
   */
//  def decode[T](ls: List[(Int, T)]): List[T] = ls.foldLeft(List())( (acc, head) => head match {
//      case (num, letter) =>
//    }
//  )

  def decode[T](ls: List[(Int, T)]): List[T] = {
    //convert each element of ls to decoded form, then call flatten on the result
    ???
  }


  /*
  p14
  Duplicate the elements of a list.
  Example:
  scala> duplicate(List('a, 'b, 'c, 'c, 'd))
  res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
   */
  def duplicate[T](input: List[T]): List[T] = {
    input match {
      case x :: xs => x :: x :: duplicate(xs)
      case _ => Nil
    }

  }


  /*
    P16 (**) Drop every Nth element from a list.

    Example:
    scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  */
  def drop[T](dropRate: Int, ls: List[T]): List[T] = {

    def recursiveDrop(rate: Int, ls: List[T]): List[T] = {
      ls match {
        case h :: tail if rate == dropRate => recursiveDrop(1, tail)
        case h :: tail => h :: recursiveDrop(rate+1, tail)
        case _ => Nil
      }
    }

    recursiveDrop(1, ls)
  }


  /*
  P17 (*) Split a list into two parts.
    The length of the first part is given. Use a Tuple for your result.
  Example:

    scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  */
  def split[T](partOneSize: Int, ls: List[T]): (List[T], List[T]) = {

    def recursiveSplit(size: Int, listParts: (List[T], List[T]) ): (List[T], List[T]) = {
      listParts._2 match {
        case h :: tail if size == partOneSize => (listParts._1 :+ h, tail)
        case h :: tail => recursiveSplit(size+1, (listParts._1 :+ h, tail))
        case _ => listParts
      }
    }

    recursiveSplit(1, (List(), ls))
  }

  /*
  P18 (**) Extract a slice from a list.
  Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
  Example:

  scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  res0: List[Symbol] = List('d, 'e, 'f, 'g)
  */
  def slice[T](start: Int, end: Int, ls: List[T]): List[T] = {

    if (start == 0 && end == 0) return ls

    val subList = ls match {
      case h :: tail if start > 0 => slice(start-1, end, tail)
      case _ => ls
    }

    if (start == 0 && end == 0) return subList

    slice(end, 0, subList.reverse).reverse
  }


  /*
    P19 (**) Rotate a list N places to the left.
    Examples:
    scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

    scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  */


  /*
    P20 (*) Remove the Kth element from a list.
    Return the list and the removed element in a Tuple. Elements are numbered from 0.
    Example:

    scala> removeAt(1, List('a, 'b, 'c, 'd))
    res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  */


  /**
  P21 (*) Insert an element at a given position into a list.
    Example:
    scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
  res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  **/
  def insertAt[T](element: T, position: Int, ls: List[T]): List[T] = {

    split(position, ls) match {
      case (front: List[T], back: List[T]) => front ::: element :: back
    }
  }


  /**
  P22 (*) Create a list containing all integers within a given range.
    Example:
    scala> range(4, 9)
  res0: List[Int] = List(4, 5, 6, 7, 8, 9)
  **/
  def range(start: Int, end: Int): List[Int] = {

    if ( start > end ) return List()

    start :: range(start + 1, end)

  }


  /*
  P23 (**) Extract a given number of randomly selected elements from a list.
  Example:
    scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
  res0: List[Symbol] = List('e, 'd, 'a)
  Hint: Use the solution to problem P20
    */

  /*
    P24 (*) Lotto: Draw N different random numbers from the set 1..M.
    Example:
    scala> lotto(6, 49)
  res0: List[Int] = List(23, 1, 17, 33, 21, 37)
  */

  /*
  P25 (*) Generate a random permutation of the elements of a list.
    Hint: Use the solution of problem P23.
  Example:

    scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
  res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
  */


}
