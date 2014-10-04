package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


	/**
	 * Link to the scaladoc - very clear and detailed tutorial of FunSuite
	 *
	 * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
	 *
	 * Operators
	 *  - test
	 *  - ignore
	 *  - pending
	 */

	/**
	 * Tests are written using the "test" operator and the "assert" method.
	 */
	test("string take") {
		val message = "hello, world"
				assert(message.take(5) == "hello")
	}

	/**
	 * For ScalaTest tests, there exists a special equality operator "===" that
	 * can be used inside "assert". If the assertion fails, the two values will
	 * be printed in the error message. Otherwise, when using "==", the test
	 * error message will only say "assertion failed", without showing the values.
	 *
	 * Try it out! Change the values so that the assertion fails, and look at the
	 * error message.
	 */
	test("adding ints") {
		assert(1 + 2 === 3)
	}


	import FunSets._

	test("contains is implemented") {
		assert(contains(x => true, 100))
	}

	/**
	 * When writing tests, one would often like to re-use certain values for multiple
	 * tests. For instance, we would like to create an Int-set and have multiple test
	 * about it.
	 * 
	 * Instead of copy-pasting the code for creating the set into every test, we can
	 * store it in the test class using a val:
	 * 
	 *   val s1 = singletonSet(1)
	 * 
	 * However, what happens if the method "singletonSet" has a bug and crashes? Then
	 * the test methods are not even executed, because creating an instance of the
	 * test class fails!
	 * 
	 * Therefore, we put the shared values into a separate trait (traits are like
	 * abstract classes), and create an instance inside each test method.
	 * 
	 */

	trait TestSets {
		val s1 = singletonSet(1)
				val s2 = singletonSet(2)
				val s3 = singletonSet(3)
	}

	/**
	 * This test is currently disabled (by using "ignore") because the method
	 * "singletonSet" is not yet implemented and the test would fail.
	 * 
	 * Once you finish your implementation of "singletonSet", exchange the
	 * function "ignore" by "test".
	 */
	test("singletonSet(1) contains 1 and not 2") {

		/**
		 * We create a new instance of the "TestSets" trait, this gives us access
		 * to the values "s1" to "s3". 
		 */
		new TestSets {
			/**
			 * The string argument of "assert" is a message that is printed in case
			 * the test fails. This helps identifying which assertion failed.
			 */
			assert(contains(s1, 1), "Singleton")

			assert(!contains(s1, 2), "Singleton with other element")
		}
	}

	test("union contains all elements") {
		new TestSets {
			val s = union(s1, s2)
					assert(contains(s, 1), "Union 1")
					assert(contains(s, 2), "Union 2")
					assert(!contains(s, 3), "Union 3")
		}
	}

	test("intersect") {
		new TestSets {
			val s = intersect(s1, s2)
					assert(!contains(s, 1), "intersect 1")
					assert(!contains(s, 2), "intersect 2")
					assert(!contains(s, 3), "intersect 3")

					val r = intersect(s1, union(s1, s2))
					assert(contains(r, 1), "intersect 1")
					assert(!contains(r, 2), "intersect 2")
					assert(!contains(r, 3), "intersect 3")
		}
	}

	test("diff") {
		new TestSets {
			val d = diff(s1, s2)
			assert(contains(d, 1), "diff 1")
			assert(!contains(d, 2), "diff 2")
			assert(!contains(d, 3), "diff 3")

			val d2 = diff(s2, s2)
			assert(!contains(d2, 1), "diff 21")
			assert(!contains(d2, 2), "diff 22")
			assert(!contains(d2, 3), "diff 23")

			val d3 = diff(union(s2,  s3), s2)
			assert(!contains(d3, 1), "diff 31")
			assert(!contains(d3, 2), "diff 32")
			assert(contains(d3, 3), "diff 33")

		}
	}

	test("filter") {
		new TestSets {
			val f = filter(s1, (x: Int) => true)
			assert(contains(f, 1), "f 1")
			assert(!contains(f, 2), "f 2")
			assert(!contains(f, 3), "f 3")
			
			def isEvenNumber(x: Int): Boolean =  2*(x/2) == x
			
			val f2 = filter(union(s1, s2), isEvenNumber)
			assert(!contains(f2, 1), "f 21")
			assert(contains(f2, 2), "f 22")
			assert(!contains(f2, 3), "f 23")
		}
	}

	def is1(x:Int) = x == 1
	def isNot1(x:Int) = !is1(x)

	test("forall") {
	  new TestSets {
	    
	    assert(forall(s1, is1))
	    assert(!forall(union(s1,s2), is1))
	    assert(!forall(s2, is1))
	    assert(forall(s2, isNot1))
	    assert(forall(union(s2, s3), isNot1))
	    
	  }
	}

	test("exists") {
		new TestSets {
			assert(exists(s1, (x: Int) => true))
			assert(exists(s1, (x: Int) => x == 1))
			assert(exists(union(s1, union(s2, s3)), (x: Int) => x == 1))
			
			assert(forall(s2, isNot1))
			assert(!exists(s2, is1))
			assert(!exists(union(s2, s3), (x: Int) => x == 1))
		}
	}
	
	test("map") {
		new TestSets {
			assert(exists(s1, (x: Int) => true))
			assert(exists(s1, (x: Int) => x == 1))
			
			val t1 = map(union(s1, union(s2, s3)), (x: Int) => 1)
			assert(contains(t1, 1))
			assert(!contains(t1, 2))
			assert(!contains(t1, 3))
			
			val t2 = map(union(s1, s2), (x: Int) => x)
			assert(contains(t2, 1))
			assert(contains(t2, 2))
			assert(!contains(t2, 3))
			
			val t3 = map(union(s1, s2), (x: Int) => 10 + x)
			assert(!contains(t3, 1))
			assert(!contains(t3, 2))
			assert(!contains(t3, 3))
			assert(contains(t3, 11))
			assert(contains(t3, 12))
			
		}
	}
	
}

