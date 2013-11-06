package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._


object scalaCheckPlay {
  
  lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(value(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)                         //> genMap: => org.scalacheck.Gen[Map[Int,Int]]
  
  genMap.sample                                   //> res0: Option[Map[Int,Int]] = Some(Map(1930454768 -> 1311431554, -1 -> 1))
  
  new QuickCheckHeap with BinomialHeap {
    println(findMin(genHeap.sample.get))
  }                                               //> -1
                                                  //| res1: quickcheck.QuickCheckHeap with quickcheck.BinomialHeap = Prop
}