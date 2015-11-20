package gmousset.yaml.parser.v1_2

import org.scalatest._

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
    parser parse filepath
  }


 /*
  "Parsing" should "be ok" in {
    val parser = new SimpleParser
    //parser parseAll(parser.command, "FORWARD 10")
    parser parseAll(parser.command, "TURN LEFT 20")
  }
  */
}
