package objsets

import java.util.NoSuchElementException

import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
    
  def matchesKeywords(keywords: List[String]): Boolean = {
    keywords.foldLeft(false)((acc, key) => acc || text.contains(key))
  }
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = {
    filterAcc(p, new Empty())
  }

  /**
   * This is a helper method for `filter` that propagates the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
   def union(that: TweetSet): TweetSet
   
  /**
   * This is a helper method for `union` that propagates the accumulated tweets.
   */
  def unionAcc(s: TweetSet, acc: TweetSet): TweetSet
   

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  def leastRetweeted: Tweet
  
  def isEmpty: Boolean

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList = {
    println("Build list ordered by retweet")
    val l = buildListByRetweet(this, Nil)
    println("Done building list ordered by retweet")
    l
  }
  
  def buildListByRetweet(input: TweetSet, list: TweetList): TweetList = {
    if (input.isEmpty) {
      list
    } else {
      val tweet = input.leastRetweeted
      val newInput = input.remove(tweet)
      if (list.isEmpty) {
    	  buildListByRetweet(newInput, new Cons(tweet, Nil))
      } else {
    	  buildListByRetweet(newInput, new Cons(tweet, list))
      }
    }
  }

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet) = that
  
  def unionAcc(s: TweetSet, acc:TweetSet) = acc
  
  def isEmpty = true

  def mostRetweeted: Nothing = throw new NoSuchElementException

  def leastRetweeted: Nothing = throw new NoSuchElementException

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    if (p(elem)) {
      val s1 = acc.incl(elem)
      val s2 = left.filterAcc(p, s1)    
      right.filterAcc(p, s2)
    } else {
      val s2 = left.filterAcc(p, acc)    
      right.filterAcc(p, s2)
    }
  }

  def union(that: TweetSet) = {
    unionAcc(this, that)
  }
  
  def unionAcc(s: TweetSet, acc:TweetSet): TweetSet = {
      s.unionAcc(left.union(right), acc.incl(elem))
  }

  def isEmpty = false

  def mostRetweeted: Tweet = {
    
    if (left.isEmpty && right.isEmpty) {
      // both children are empty
      elem
    } else if (left.isEmpty) {
      // left is empty, right is not
      val rightMost = right.mostRetweeted
      if (elem.retweets > rightMost.retweets) {
        elem
      } else {
        rightMost
      }
    } else if (right.isEmpty) {
      // right is empty, left is not
      val leftMost = left.mostRetweeted
      if (elem.retweets > leftMost.retweets) {
        elem
      } else {
        leftMost
      }
    } else {
      // neither of the children is empty
      val leftMost = left.mostRetweeted
      val rightMost = right.mostRetweeted
      if (elem.retweets > leftMost.retweets && elem.retweets > rightMost.retweets) {
        elem
      } else if (leftMost.retweets > rightMost.retweets) {
        leftMost
      } else {
        rightMost
      }
    }

  }

  def leastRetweeted: Tweet = {
    
    if (left.isEmpty && right.isEmpty) {
      // both children are empty
      elem
    } else if (left.isEmpty) {
      // left is empty, right is not
      val rightMost = right.leastRetweeted
      if (elem.retweets < rightMost.retweets) {
        elem
      } else {
        rightMost
      }
    } else if (right.isEmpty) {
      // right is empty, left is not
      val leftMost = left.leastRetweeted
      if (elem.retweets < leftMost.retweets) {
        elem
      } else {
        leftMost
      }
    } else {
      // neither of the children is empty
      val leftMost = left.leastRetweeted
      val rightMost = right.leastRetweeted
      if (elem.retweets < leftMost.retweets && elem.retweets < rightMost.retweets) {
        elem
      } else if (leftMost.retweets < rightMost.retweets) {
        leftMost
      } else {
        rightMost
      }
    }

  }

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = getTweetSetWithKeywords(google)
  lazy val appleTweets: TweetSet =  getTweetSetWithKeywords(apple)
  
  def getTweetSetWithKeywords(keywords: List[String]) = {
    println("Getting tweets with keywords: " + keywords)
    val s = TweetReader.allTweets.filter(t => t.matchesKeywords(keywords))
    println("Done getting tweets with keywords: " + keywords)
    s
  }
  
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = (googleTweets union appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
