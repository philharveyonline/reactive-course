package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  // TODO style check
  // TODO remove comments etc


  property("findMin of heap containing one item returns that item") =
    forAll { a: Int =>
      val h = insert(a, empty)
      findMin(h) == a
    }


  property("deleteMin of heap containing one item returns empty heap") =
    forAll { a: Int =>
      isEmpty(deleteMin(insert(a, empty)))
    }


  property("after inserting the existing minimum item, findMin returns that item") =
    forAll { (h: H) =>
      val m = if(isEmpty(h)) 0 else findMin(h)
      findMin(insert(m, h)) == m
    }


  property("after inserting two items into an empty heap, findMin should return the smallest of the two items") =
    forAll { (x: Int, y: Int) =>
      val heapWithX = insert(x, empty)
      val heapWithXAndY = insert(y, heapWithX)

      findMin(heapWithXAndY) == math.min(x, y)
    }


  property("after inserting an item into an empty heap, then delete the minimum, the resulting heap should be empty.") =
    forAll { (x: Int) =>
      isEmpty(deleteMin(insert(x, empty)))
  }

  property("after inserting two identical items into an empty heap, then delete the minimum, the resulting heap should not be empty.") =
    forAll { (x: Int) =>
       val afterDoubleInsert = insert(x, insert(x, empty))
       val afterFirstDelete = deleteMin(afterDoubleInsert)
       val afterSecondDelete = deleteMin(afterFirstDelete)
      !isEmpty(afterFirstDelete) && isEmpty(afterSecondDelete)
  }


  def minIncreases(h: H): Boolean = {
    isEmpty(h) || minIncreasesRecurse(true, findMin(h), h)
  }

  def minIncreasesNoDups(h: H): Boolean = {
    isEmpty(h) || minIncreasesRecurse(false, findMin(h), deleteMin(h))
  }


  def minIncreasesRecurse(dupsOk: Boolean, previousMin: Int, h: H): Boolean = {
    if (isEmpty(h)) {
      true
    } else {
      val m = findMin(h)
      val minMeetsExpectation = if(dupsOk) previousMin <= m else previousMin < m
      minMeetsExpectation && minIncreasesRecurse(dupsOk, m, deleteMin(h))
    }
  }

  property("you should get a sorted sequence of elements when continually finding and deleting minima") =
    forAll { (h: H) =>
      minIncreases(h)
    }

  property("after melding you should get a sorted sequence of elements") =
    forAll { (h1: H, h2: H) =>
      val h = meld(h1, h2)
      minIncreases(h)
    }


  property("a heap with no duplicates should return monotonically increasing values from deleteMin/findMin") =
    forAll { (intSet: Set[Int]) =>

      val h = intSet.foldLeft(empty)( (h: H, x: Int) => insert(x, h))
      minIncreasesNoDups(h)
    }


  property("finding a minimum of the melding of any two heaps should return a minimum of one or the other") =
    forAll { (h1: H, h2: H) =>
      val meldedMin = findMin(meld(h1, h2))
      meldedMin == findMin(h1) || meldedMin == findMin(h2)
    }


  property("melding two empty heaps returns an empty heap") = isEmpty(meld(empty, empty))


  property("melding the min and the deleteMin yields the original min") =
    forAll { (h: H) =>
      val m = findMin(h)
      val rest = deleteMin(h)
      val singleton = insert(m, empty)
      m == findMin(meld(rest, singleton))
    }



  property("melding with an empty heap leaves minimum unchanged") =
    forAll { (h: H) =>
      val meldLeft = meld(h, empty)
      val meldRight = meld(empty, h)
      val m = findMin(h)
      m == findMin(meldLeft) && m == findMin(meldRight)
  }


  property("melding an empty heap with another leaves minimum unchanged") =
    forAll { (h: H) =>
      findMin(h) == findMin(meld(empty, h))
  }


  private def size(h: H): Int = {
    def sizeRecursive(count: Int, h: H): Int = {
      if(isEmpty(h)) {
        count
      }
      else {
        sizeRecursive(count + 1, deleteMin(h))
      }
    }
    sizeRecursive(0, h)
  }

  property("melding two heaps gives a heap of the correct size") =
    forAll { (h1: H, h2: H) =>
      size(h1) + size(h2) == size(meld(h1, h2))
    }

  property("inserting into a  heap increments its size") =
    forAll { (h: H, x: Int) =>
      size(insert(x, h)) == size(h) + 1
    }


  property("deleting from a heap decrements its size") =
    forAll { (h: H) =>
      size(deleteMin(h)) == size(h) - 1
    }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
