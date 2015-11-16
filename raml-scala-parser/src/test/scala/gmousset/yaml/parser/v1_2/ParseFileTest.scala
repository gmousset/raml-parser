package gmousset.yaml.parser.v1_2

import org.scalatest._

import scala.io.Source

/**
  * Created by Gwendal Mousset on 13/11/2015.
  */
class ParseFileTest extends FlatSpec with Matchers {

  "Parsing" should "be ok" in {
    // open file

    val l1 = List("a" -> "z")
    println(l1)

    val filename = "/test01.yaml"
    val source = Source.fromURL(getClass().getResource(filename))
    val parser = new YamlParser(source)
    val result = parser parse()
    result should be (true)
  }
}
