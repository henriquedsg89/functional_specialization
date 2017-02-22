package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h: H, a: Int, b: Int) =>
    val m = findMin(insert(a, insert(b, h)))
    if (isEmpty(h)) {
      if (a < b) m == a
      else m == b
    } else {
      val initialM = findMin(h)

      if (initialM < a && initialM < b) initialM == m
      else if (initialM < a && initialM >= b) b == m
      else if (initialM >= a && initialM < b) a == m
      else if (a < b) a ==  m
      else b == m
    }
  }

  property("gen3") = forAll { (h: H) =>
    if (isEmpty(h)) {
      isEmpty(deleteMin(insert(1, h)))
    } else {
      val m = findMin(h)
      findMin(deleteMin(insert(m - 1, h))) == m
    }
  }
}
