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
  
  test("times with hello world") {
    val dict = times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')).toMap
    assert(dict('e') == 1)
    assert(dict(' ') == 1)
    assert(dict(',') == 1)
    assert(dict('l') == 3)
    assert(dict('o') == 2)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a very short text should be identity -- the same using the french tree") {
    new TestTrees {
      assert(decode(frenchCode, encode(frenchCode)("ab".toList)) === "ab".toList)
    }
  }
  
  test("code tree to table") {
    new TestTrees {
      val table1 = convert(t1).toMap
      assert(table1.size == 2, "size: " +  table1.size)
      assert(table1('a') == List(0) , "a: " + table1('a'))
      assert(table1('b') == List(1), "b: " + table1('b'))
      
      val table2 = convert(t2).toMap
      assert(table2.size == 3, "size: " +  table2.size)
      assert(table2('a') == List(0, 0) , "a: " + table2('a'))
      assert(table2('b') == List(0, 1), "b: " + table2('b'))
      assert(table2('d') == List(1) , "d: " + table2('a'))
    }
  }
  
  test("quickEncode") {
    val text = string2Chars("huffmantrescool")
    val encoded = encode(frenchCode)(text)
    val qEncoded = quickEncode(frenchCode)(text)
    
    /*
    println("Input: " + text)
    p7rintln("enc:   " + encoded + " => " + decode(frenchCode, encoded))
    println("quick: " + qEncoded + " => " + decode(frenchCode, qEncoded))
    println("decode: [" + decode(frenchCode, List(0, 0, 1)) + "]")
    */
    
    assert(encoded == qEncoded)
    assert(decode(frenchCode, qEncoded) == text)
  }
  
  
  
}
