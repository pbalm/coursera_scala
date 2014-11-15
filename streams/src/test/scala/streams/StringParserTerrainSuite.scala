package streams

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import streams.StringParserTerrain
import streams.BloxorzSuite

@RunWith(classOf[JUnitRunner])
class StringParserTerrainSuite extends FunSuite {
	
	class TestTerrain extends StringParserTerrain {
		val level =
			"""ooo-------
			|oSoooo----
			|ooooooooo-
			|-ooooooooo
			|-----ooToo
			|------ooo-""".stripMargin
			
	}

	test("terrain function") {
	  new TestTerrain {
	    
	  }

	}


}