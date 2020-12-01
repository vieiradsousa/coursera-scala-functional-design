package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    for {
      x <- arbitrary[A]
      h <- oneOf(Gen.const(empty), genHeap)
    } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("delete min from 1 elem results empty") =
    forAll { a: Int =>
      val h1 = insert(a, empty)
      deleteMin(h1) == empty
    }

  property("delete min 2 elems results empty") =
    forAll { (a: Int, b: Int, c: Int, d: Int) =>
      val h1 = insert(a, insert(b, empty))
      deleteMin(deleteMin(h1)) == empty
    }

  property("min meld is min of one or another") =
    forAll { (h1: H, h2: H) =>
      val hm = meld(h1, h2)
      findMin(hm) == Math.min(findMin(h1), findMin(h2))
    }

  property("meld empty results same heap") =
    forAll { (h: H) =>
      meld(h, empty) == h
  }

  property("meld empty results same heap") =
    forAll { (h: H) =>
      meld(empty, h) == h
  }

  property("2 elem, minimum are min of the two") =
    forAll { (a: Int, b: Int) =>
      val h = insert(a, insert(b, empty))
      findMin(h) == Math.min(a, b)
  }

  property("melding 3 times and deleting 3 mins, next min are equal") =
    forAll { (h: H) =>
      val hm = meld(meld(h, h), h)
      val h1 = deleteMin(deleteMin(deleteMin(hm)))
      val h2 = deleteMin(h)
      isEmpty(h2) || findMin(h1) == findMin(h2)
  }
}
