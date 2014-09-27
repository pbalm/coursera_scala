package greeter


object worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

	val x = 1                                 //> x  : Int = 1
	
	def increase(i: Int) = i * 1              //> increase: (i: Int)Int
	increase(x)                               //> res0: Int = 1
	
	var row = List[Int](1)                    //> row  : List[Int] = List(1)
	
}