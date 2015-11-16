package gmousset.yaml.parser.v1_2

import org.slf4j.LoggerFactory

import scala.io.Source
import scala.util.parsing.combinator._

/**
  * Created by Gwendal Mousset on 13/11/2015.
  */
class YamlParser(source: Source) extends RegexParsers {

  val logger = LoggerFactory.getLogger(this.getClass)

  // Character Encodings
  def cByteOrderMask:Parser[String] = "\\uFEFF"

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
    cSingleQuote | cDoubleQuote | cDirective | cReserved
  def cFlowIndicator:Parser[String] = cCollectEntry | cSequenceStart | cSequenceEnd | cMappingStart | cMappingEnd



  // Line Break Characters
  def bLineFeed:Parser[String] = "\\u000A"
  def bCarriageReturn:Parser[String] = "\\u000D"
  def bChar:Parser[String] = bLineFeed | bCarriageReturn
  def nbChar:Parser[String] = "\\u0009" | "[\\u0020-\\u007E]" | "\\u0085" |
    "[\\u00A0-\\uD7FF]" | "[\\uE000-\\uFFFD]" | "[\\u10000-\\u10FFFF]"  // order mask not removed
  def bBreak:Parser[String] = bCarriageReturn | bLineFeed | bCarriageReturn | bLineFeed
  def bAsLineFeed:Parser[String] = bBreak
  def bNonContent:Parser[String] = bBreak

  // White Space Characters
  def sSpace:Parser[String] = "\\u0020"
  def sTab:Parser[String] = "\\u0009"
  def sWhite:Parser[String] = sSpace | sTab
  def nsChar:Parser[String] = "[\\u0021-\\u007E]" | "\\u0085" |
    "[\\u00A0-\\uD7FF]" | "[\\uE000-\\uFFFD]" | "[\\u10000-\\u10FFFF]"

  // Miscellaneous Characters
  def nsDecDigit:Parser[String] = "[\\u0030-\\u0039]"
  def nsHexDigit:Parser[String] = nsDecDigit | "[\\u0041-\\u0046]" | "[\\u0061-\\u0066]"
  def nsAsciiLetter:Parser[String] = "[\\u0041-\\u005A]" | "[\\u0061-\\u007A]"
  def nsWordChar:Parser[String] = nsDecDigit | nsAsciiLetter | "-"
  def nsUriChar:Parser[Any] = "%" ~ nsHexDigit ~ nsHexDigit | nsWordChar | "#" | ";" | "/" | "?" | ":" | "@" | "&" |
    "=" | "+" | "$" | "," | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")" | "[" | "]"
  def nsTagChar:Parser[Any] = "%" ~ nsHexDigit ~ nsHexDigit | nsWordChar | "#" | ";" | "/" | "?" | ":" | "@" | "&" |
    "=" | "+" | "$" | "_" | "." | "~" | "*" | "'" | "(" | ")"

  // Escaped Characters
  def cEscape:Parser[String] = "\\"
  def nsEscNull:Parser[String] = "0"
  def nsEscBell:Parser[String] = "a"
  def nsEscBackspace:Parser[String] = "b"
  def nsEscHorizontalTab:Parser[String] = "t" | "\\u0009"
  def nsEscLineFeed:Parser[String] = "n"
  def nsEscVerticalTab:Parser[String] = "v"
  def nsEscFormFeed:Parser[String] = ""
  def nsEsc:Parser[String] = ""
  def nsEsc:Parser[String] = ""
  def nsEsc:Parser[String] = ""
  def nsEsc:Parser[String] = ""
  def nsEsc:Parser[String] = ""
  def nsEsc:Parser[String] = ""
  def nsEsc:Parser[String] = ""
  def nsEsc:Parser[String] = ""
  def nsEsc:Parser[String] = ""
  def nsEsc:Parser[String] = ""
  def nsEsc:Parser[String] = ""
  def nsEsc:Parser[String] = ""
  def nsEsc:Parser[String] = ""




  def parse(): Boolean = {
    /*
    for (line <- source.getLines()) {
      parse(cMappingValue, line) match {
        case Success(matched, _) => logger.debug(matched)
        case _ => Unit
      }
    }
    */
    true
  }
}
