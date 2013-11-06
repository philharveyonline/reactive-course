package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._


object scalaCheckPlay {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(287); 
  
  lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(value(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v);System.out.println("""genMap: => org.scalacheck.Gen[Map[Int,Int]]""");$skip(19); val res$0 = 
  
  genMap.sample;System.out.println("""res0: Option[Map[Int,Int]] = """ + $show(res$0));$skip(89); val res$1 = 
  
  new QuickCheckHeap with BinomialHeap {
    println(findMin(genHeap.sample.get))
  };System.out.println("""res1: quickcheck.QuickCheckHeap with quickcheck.BinomialHeap = """ + $show(res$1))}
}
