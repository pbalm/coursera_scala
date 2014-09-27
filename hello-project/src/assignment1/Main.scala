package assignment1

import scala.collection.mutable.ListBuffer

object Main extends App {
  
  def pascal(c: Int, r: Int): Int = {
    
    def updateToNextRow(row: List[Int]): List[Int] = {
     
      var newRow = new ListBuffer[Int]
      var prev = 0;
      for (element <- row) {
        newRow += prev + element
        prev = element
      }
      newRow += 1
      newRow.toList
    }
    
    def getRow(row: List[Int]): List[Int] = {
      if (row.length > r) row
      else getRow(updateToNextRow(row))
    }
    
    getRow(List(1))(c)
  }
  
  def balance(chars: List[Char]): Boolean = {
    //println(chars.mkString)
    
    val OPEN = '('
    val CLOSE = ')'
    
    // Convert to 1 for opening bracket, -1 for closing and 0 otherwise
    val nums = chars.map(c => {
      c match {
      case OPEN => 1
      case CLOSE => -1
      case _ => 0
       }
    })
    
    val result = nums.foldLeft(0)((acc, x) => {
      //print("%d ".format(acc))
      if (acc < 0) -1 // Negative indent is error condition, maintain it
      else acc + x
    })

    result == 0
  }
  
  def testbalance() = {
	  println("\nShould be true : " + balance("(if (zero? x) max (/ 1 x))".toList))
	  println("\nShould be true : " + balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
	  println("\nShould be false: " + balance(":-)".toList))
	  println("\nShould be false: " + balance("())(".toList))
  }  
    
  def countChange(money: Int, coins: List[Int]): Int = {
    
    def walkTree(remainder: Int, lastCoin: Int, poss: Int): Int = {
    	println("remainder: %d  last coin: %d  poss: %d".format(remainder, lastCoin, poss))
      
		def checkCoin(poss: Int, coin: Int): Int = {
		    if (coin < lastCoin || remainder - coin < 0) {
		      poss
		    } else if (remainder - coin > 0) { 
		      walkTree(remainder - coin, coin, poss)
		    } else {
		      // exact change
		      println("poss: " + (poss + 1))
		      poss + 1
		    }
		}
      
    	coins.foldLeft(poss)(checkCoin)
    }
    
    walkTree(money, 0, 0)
  }
  
  def testCount() {
    println(countChange(6, List(1, 2)))
  }
  
  testCount()
  
}