package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}

  test("makeCodeTree") {
    new TestTrees {
      assert(encode(createCodeTree("bbbaadddd".toList))("bab".toList) === List(0,1,0,0,0,1))
    }
  }

  test("decode and encode longer strings using frenchcode and quickEncode") {
    assert(decode(frenchCode, quickEncode(frenchCode)("thisisatest".toList)) === "thisisatest".toList)
  }

  test("encode and decode a sample string using new custome tree") {
    val myText = "thisisatestofyourcreatecodetreemethod".toList
    val myTree = createCodeTree(myText)
    assert(decode(myTree, encode(myTree)(myText)) === myText)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    assert(times(string2Chars("batata")) === List(('a', 3), ('b', 1), ('t', 2)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode text") {
    new TestTrees {
      val a = decode(t1, List(0, 1))
      assert(a === List('a', 'b'))
    }
  }

  test("decode text2") {
    new TestTrees {
      val a = decode(t2, List(0, 0, 0, 1))
      assert(a === List('a', 'b'))
    }
  }

  test("encode text") {
    new TestTrees {
      val a = encode(t1)(List('a', 'b'))
      assert(a === List(0, 1))
    }
  }

  test("encode text2") {
    new TestTrees {
      val a = encode(t2)(List('a', 'b'))
      assert(a === List(0, 0, 0, 1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a decent text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("dabbad".toList)) === "dabbad".toList)
    }
  }

  test("convert table") {
    new TestTrees {
     val a = convert(t1)
      assert(a === List(('a', List(0)), ('b', List(1))))
    }
  }

  test("quick encode gives the correct byte sequence") {
    new TestTrees {
      assert(quickEncode(t1)("ab".toList) === List(0, 1))
    }
  }

  test("encode and decode a sample string using new custome tree2") {
    val myText = (1 to 3000).map(_ ⇒ (Math.random() * 10).toInt).mkString.toList
    val myTree = createCodeTree(myText)
    assert(decode(myTree, encode(myTree)(myText)) === myText)
  }

  /*
  [Test Description] 'createCodeTree(someText)' gives an optimal encoding, the number of bits when encoding 'someText' is minimal
  [Observed Error] test has been aborted
  [Lost Points] 15

  [Test Description] 'createCodeTree(someText)' gives an optimal encoding, the number of bits when encoding 'someText' is minimal
  [Observed Error] 1 did not equal 1919
  [Lost Points] 15

  [Test Description] quick encode gives the correct byte sequence
  [Observed Error] List(0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1) did not equal List(1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1)
  [Lost Points] 20

  [Test Description] encode some text with frenchCode
  [Observed Error] List(1, 1, 0, 1, 1, 1, 1) did not equal List(1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1)
  [Lost Points] 20

  [Test Description] decode and encode a very short text with a larger tree
  [Observed Error] List('a', ' ') did not equal List('a', 'b', 'u')
  [Lost Points] 10

  [Test Description] convert: code table is created correctly
  [Observed Error] List((a,List(0)), (b,List(1)), (d,List(1))) did not equal List((a,List(0, 0)), (b,List(0, 1)), (d,List(1)))
  [Lost Points] 20

  [Test Description] decode and quick encode is identity
  [Observed Error] List(' ', ' ', '2', ' ', 't', 't', 'r', 'o', 'm', 't', 'L', 'a', 'c') did not equal List('t', 'u', 'r', 'e', ' ', 'f', 'r', 'o', 'm', ' ', '4', '5', ' ', 'B', 'C', ',', ' ', 'm', 'a', 'k', 'i', 'n', 'g', ' ', 'i', 't', ' ', 'o', 'v', 'e', 'r', ' ', '2', '0', '0', '0', ' ', 'y', 'e', 'a', 'r', 's', ' ', 'o', 'l', 'd', '.', ' ', 'R', 'i', 'c', 'h', 'a', 'r', 'd', ' ', 'M', 'c')
  [Lost Points] 10

  [Test Description] decode and encode some longer text should be identity
  [Observed Error] List(' ') did not equal List('l', 'i', 't', 'e', 'r', 'a', 't', 'u', 'r', 'e', ' ', 'f', 'r', 'o', 'm', ' ', '4', '5', ' ', 'B', 'C', ',', ' ', 'm', 'a', 'k', 'i', 'n', 'g', ' ', 'i', 't', ' ', 'o', 'v', 'e', 'r', ' ', '2', '0', '0', '0', ' ', 'y', 'e', 'a', 'r', 's', ' ', 'o', 'l', 'd', '.')
  [Lost Points] 10

   */
}
