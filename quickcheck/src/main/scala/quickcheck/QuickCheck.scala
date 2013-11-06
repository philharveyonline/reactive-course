package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  // TODO style check
  // TODO remove comments etc
  // TODO what does findMin of an empty heap return?

  
  property("findMin of heap containing one item returns that item") =
    forAll { a: Int =>
      val h = insert(a, empty)
      findMin(h) == a
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

  private def recursivelyCheckThatMinIncreases(previousMin: Int, h: H): Boolean =
  {
    if (isEmpty(h)) {
      true
    } else {
      val m = findMin(h)
      previousMin <= m && recursivelyCheckThatMinIncreases(m, deleteMin(h))
    }
  } 
  
  property("you should get a sorted sequence of elements when continually finding and deleting minima") =
    forAll { (h: H) =>
      recursivelyCheckThatMinIncreases(findMin(h), h)
    }
 
  
  property("finding a minimum of the melding of any two heaps should return a minimum of one or the other") = 
    forAll { (h1: H, h2: H) =>
      val meldedMin = findMin(meld(h1, h2))
      meldedMin == findMin(h1) || meldedMin == findMin(h2)  
    }
  
  
  
  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
