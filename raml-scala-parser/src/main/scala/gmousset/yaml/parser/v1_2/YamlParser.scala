package gmousset.yaml.parser.v1_2

import org.slf4j.LoggerFactory

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by Gwendal Mousset on 13/11/2015.
  */
class YamlParser(source: Source) extends RegexParsers {

  val logger = LoggerFactory.getLogger(this.getClass)

  def parse(): Boolean = {
    for (line <- source.getLines()) {

      parse(mappingValue, line) match {
        case Success(matched, _) => logger.debug(matched)
        case _ => Unit
      }
    }
    true
  }

  def documentMarker:Parser[String] = "---"

  // Character Set
  def cPrintable:Parser[String] = "\\u0009" | "\\u000A" | "\\u000D" | "[\\u0020-\\u007E]" | "\\u0085" |
    "[\\u00A0-\\uD7FF]" | "[\\uE000-\\uFFFD]" | "[\\u10000-\\u10FFFF]"
  def nbJson:Parser[String] = "\\u0009" | "[\\u0020-\\u10FFFF]"

  // Indicators
  def cSequenceEntry:Parser[String] = "-"
  def cMappingKey:Parser[String] = "?"
  def cMappingValue:Parser[String] = ":"
  def cCollectEntry:Parser[String] = ","
  def cSequenceStart:Parser[String] = "["
  def cSequenceEnd:Parser[String] = "]"
  def cMappingStart:Parser[String] = "{"
  def cMappingEnd:Parser[String] = "}"
  def cComment:Parser[String] = "#"
  def cAnchor:Parser[String] = "&"
  def cAlias:Parser[String] = "*"
  def cTag:Parser[String] = "!"
  def cLiteral:Parser[String] = "|"
  def cFolded:Parser[String] = ">"
  def cSingleQuote:Parser[String] = "'"
  def cDoubleQuote:Parser[String] = "\""
  def cDirective:Parser[String] = "%"
  def cReserved:Parser[String] = "@" | "`"
  def cIndicator:Parser[String] = cSequenceEntry | cMappingKey | cMappingValue | cCollectEntry | cSequenceStart |
    cSequenceEnd | cMappingStart | cMappingEnd | cComment | cAnchor | cAlias | cTag | cLiteral | cFolded |
    cSingleQuote | cDoubleQuote | cDirective | cReserved | cIndicator
  def cFlowIndicator:Parser[String] = cCollectEntry | cSequenceStart | cSequenceEnd | cMappingStart | cMappingEnd

  // Line Break Characters
  def bLineFeed:Parser[String] = "\\u000A"
  def bCarriageReturn:Parser[String] = "\\u000D"
  def bChar:Parser[String] = bLineFeed | bCarriageReturn
  def bBreak:Parser[String] = "\\u000D\\u000A" | bCarriageReturn | bLineFeed
  def bAsLineFeed:Parser[String] = bBreak
  def bNonContent:Parser[String] = bBreak

  // White Space Characters
  def sSpace:Parser[String] = "\\u0020"
  def sTab:Parser[String] = "\\u0009"
  def sWhite:Parser[String] = sSpace | sTab
  def nsChar:Parser[String] = ""
}
