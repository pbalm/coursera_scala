package recfun
import common._
import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
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

  /**
   * Exercise 2
   */
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

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    def walkTree(remainder: Int, lastCoin: Int, poss: Int): Int = {
    	//println("remainder: %d  last coin: %d  poss: %d".format(remainder, lastCoin, poss))
      
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

}
