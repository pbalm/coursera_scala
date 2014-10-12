package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val a = new Tweet("a", "a body", 20)
    val b = new Tweet("b", "b body", 20)
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    
    val set1 = new Empty
    val set2 = set1.incl(a) // [a]
    val set3 = set2.incl(b) // [a, b]
    val set4c = set3.incl(c) // [a, b, c]
    val set4d = set3.incl(d) // [a, b, d]
    val set5 = set4c.incl(d) // [a, b, c, d]
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: set not rooted at a but at c") {
    new TestSets {
      val setC = set1.incl(c).incl(d).incl(a)
      val setB = set1.incl(b)
      assert(size(setC.union(setB)) === 4)
      assert(size(setB.union(setC)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }
  
  test("most retweeted") {
    new TestSets {
      intercept[NoSuchElementException] {
        set1.mostRetweeted
      }
      
      assert(set4c.mostRetweeted.user == "b")
      assert(set1.incl(c).incl(d).mostRetweeted.user == "d")
    }
  }

  test("least retweeted") {
    new TestSets {
      intercept[NoSuchElementException] {
        set1.mostRetweeted
      }
      
      assert(set4c.leastRetweeted.user == "c", "set 4c: " +  set4c.leastRetweeted)
      assert(set1.incl(c).incl(d).leastRetweeted.user == "c", "set [c,d]")
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty, "list is empty")
      assert(trends.head.user == "a" || trends.head.user == "b", "user of first tweet is not a nor b")
    }
  }
  
  test("Tweet.matches") {
    new TestSets {
      assert(d.matchesKeywords(List("a", "bb", "body", "x")), "d matches body")
      assert(d.matchesKeywords(List("d", "bb", "body", "x")), "d matches d")
      assert(!d.matchesKeywords(List("a", "c", "x")), "d matches nothing: " + d.text)
    }
  }
}
