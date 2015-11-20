package gmousset.yaml.parser.v1_2

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by Gwendal Mousset on 20/11/2015.
  */
class SimpleParser extends RegexParsers{

  def command:Parser[Any] = log(forward | turn)("command")
  def forward:Parser[Any] = log("FORWARD" ~ distance)("forward")
  def distance:Parser[Any] = log("[0-9]+".r)("distance")
  def turn:Parser[Any] = log("TURN" ~ direction ~ angle)("turn")
  def direction:Parser[Any] = log("LEFT" | "RIGHT")("direction")
  def angle:Parser[Any] = log("[0-9]+".r)("angle")
}
