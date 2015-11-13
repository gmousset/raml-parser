package gmousset.yaml.parser.v1_2

import org.scalatest._

import scala.io.Source

/**
  * Created by Gwendal Mousset on 13/11/2015.
  */
class ParseFileTest extends FlatSpec with Matchers {

  "Parsing" should "be ok" in {
    // open file

    val l1 = List("a", "b", "c", "d")
    val l2 = List("a", "b")
    val l3 = l1 diff l2
    println(l1 + " - " + l2 + " = " + l3)

    val filename = "/test01.yaml"
    val source = Source.fromURL(getClass().getResource(filename))
    val parser = new YamlParser(source)
    val result = parser parse()
    result should be (true)
  }
}
