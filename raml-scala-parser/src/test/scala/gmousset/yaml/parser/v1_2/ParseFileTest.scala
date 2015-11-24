package gmousset.yaml.parser.v1_2

import org.scalatest._

//import scala.util.parsing.combinator.Parsers.ParseResult


/**
  * Created by Gwendal Mousset on 13/11/2015.
  */
class ParseFileTest extends FlatSpec with Matchers {


  "Parsing" should "be ok" in {
    // open file
    val filename = "/test01.yaml"
    //val filename = "/pp.yaml"
    val filepath:String = getClass.getResource(filename).getPath
    val parser = new YamlParser
//    parser.parseString("---") shouldBe true
    parser.parse(filepath)
  }


/*
  "Parsing" should "be ok" in {
    val parser = new SimpleParser
    parser.parseString("FORWARD 10") shouldBe true
    //val result:Boolean = parser parseString("TURN LEFT 20")
  }
*/
}
