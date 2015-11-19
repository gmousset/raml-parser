package gmousset.yaml.parser.v1_2

import org.slf4j.LoggerFactory

import scala.io.Source
import scala.util.parsing.combinator._

/**
  * Created by Gwendal Mousset on 13/11/2015.
  */
class YamlParser(source: Source) extends RegexParsers {

  val logger = LoggerFactory.getLogger(this.getClass)

  /*
   * Characters
   */
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
  def nsEscFormFeed:Parser[String] = "f"
  def nsEscCarriageReturn:Parser[String] = "r"
  def nsEscEscape:Parser[String] = "e"
  def nsEscSpace:Parser[String] = "\\u0020"
  def nsEscDoubleQuote:Parser[String] = """"""
  def nsEscSlash:Parser[String] = "/"
  def nsEscBackslash:Parser[String] = "\\"
  def nsEscNextLine:Parser[String] = "N"
  def nsEscNonBreakingSpace:Parser[String] = "_"
  def nsEscLineSeparator:Parser[String] = "L"
  def nsEscParagraphSeparator:Parser[String] = "P"
  def nsEsc8Bit:Parser[String] = "x"
  def nsEsc16Bit:Parser[String] = "u"
  def nsEsc32Bit:Parser[String] = "U"
  def cNsEscChar:Parser[Any] = "\\" ~ (nsEscNull | nsEscBell | nsEscBackspace | nsEscHorizontalTab | nsEscLineFeed |
    nsEscVerticalTab | nsEscFormFeed | nsEscCarriageReturn | nsEscEscape | nsEscSpace | nsEscDoubleQuote | nsEscSlash |
    nsEscBackslash | nsEscNextLine | nsEscNonBreakingSpace | nsEscLineSeparator | nsEscParagraphSeparator |
    nsEsc8Bit | nsEsc16Bit | nsEsc32Bit)


  /*
   * Basic Structures
   */

  // Indentation Spaces
  def sIndent(n: Int):Parser[Any] = repN(n, sSpace)
  def sIndentLessThan(n: Int):Parser[Any] = {
    rep(sSpace) match {
      case Success(list:List, _) if list.length < n => Parser {Failure("failure", _)}
      case _:ParseResult => _
    }
  }
  def sIndentEqualOrLessThan(n: Int):Parser[Any] = {
    rep(sSpace) match {
      case Success(list:List, _) if list.length <= n => Parser {Failure("failure", _)}
      case _:ParseResult => _
    }
  }

  // Separation Spaces
  def sSeparateInLine:Parser[Any] = rep1(sWhite)  // TODO:must add start of line

  // Line Prefixes
  def sLinePrefix(n: Int, c:Context):Parser[Any] =  c match  {
    case BlockOut => sBlockLinePrefix(n)
    case BlockIn  => sBlockLinePrefix(n)
    case FlowOut  => sFlowLinePrefix(n)
    case FlowIn   => sFlowLinePrefix(n)
  }

  def sBlockLinePrefix(n: Int):Parser[Any] = sIndent(n)
  def sFlowLinePrefix(n: Int):Parser[Any] = sIndent(n) ~ opt(sSeparateInLine)

  // Empty Lines
  def lEmpty(n: Int, c: Context):Parser[Any] = sLinePrefix(n, c) | sIndentLessThan(n) | bAsLineFeed

  // Line Folding
  def bLTrimmed(n: Int, c: Context):Parser[Any] = bNonContent ~ lEmpty(n, c)
  def bAsSpace:Parser[Any] = bBreak
  def bLFolded(n: Int, c: Context):Parser[Any] = bLTrimmed(n, c) | bAsSpace
  def sFlowFolded(n: Int):Parser[Any] = opt(sSeparateInLine) ~ bLFolded(n, FlowIn("flow-in")) ~ sFlowLinePrefix(n)


  // Comments
  def cNbCommentText:Parser[Any] = "#" ~ rep(nbChar)
  def bComment:Parser[Any] = bNonContent // TODO: must add end of file
  def sBComment:Parser[Any] = opt(sSeparateInLine ~ opt(cNbCommentText)) ~ bComment
  def lComment:Parser[Any] = sSeparateInLine ~ opt(cNbCommentText) ~ bComment
  def sLComments:Parser[Any] = (sBComment /* TODO: must add start of line */) ~ rep(sLComments)

  // Separation Lines
  def sSeparate(n: Int, c: Context):Parser[Any] = c match {
    case BlockOut => sSeparateLines(n)
    case BlockIn  => sSeparateLines(n)
    case FlowOut  => sSeparateLines(n)
    case FlowIn   => sSeparateLines(n)
    case BlockKey => sSeparateInLine
    case FlowKey  => sSeparateInLine
  }

  def sSeparateLines(n: Int):Parser[Any] = (sLComments ~ sFlowLinePrefix(n)) | sSeparateInLine

  // Directives
  def lDirective:Parser[Any] = "%" ~ (nsYamlDirective | nsTagDirective | nsReservedDirective) ~ sLComments
  def nsReservedDirective:Parser[Any] = nsDirectiveName ~ rep(sSeparateInLine ~ nsDirectiveParameter)
  def nsDirectiveName:Parser[Any] = rep1(nsChar)
  def nsDirectiveParameter:Parser[Any] = rep1(nsChar)

    // YAML Directives
  def nsYamlDirective:Parser[Any] = "Y" ~ "A" ~ "M" ~ "L" ~ sSeparateInLine ~ nsYamlVersion
  def nsYamlVersion:Parser[Any] = rep1(nsDecDigit) ~ "." ~ rep1(nsDecDigit)
    // Tag Directives
  def nsTagDirective:Parser[Any] = "T" ~ "A" ~ "G" ~ sSeparateInLine ~ cTagHandle ~ sSeparateInLine ~ nsTagPrefix
    // Tag Handles
  def cTagHandle:Parser[Any] = cNamedTagHandle | cSecondaryTagHandle | cPrimaryTagHandle
  def cPrimaryTagHandle:Parser[Any] = "!"
  def cSecondaryTagHandle:Parser[Any] = "!" ~ "!"
  def cNamedTagHandle:Parser[Any] = "!" ~ rep1(nsWordChar) ~ "!"
    // Tag Prefixes
  def nsTagPrefix:Parser[Any] = cNsLocalTagPrefix | nsGlobalTagPrefix
  def cNsLocalTagPrefix:Parser[Any] = "!" ~ rep(nsUriChar)
  def nsGlobalTagPrefix:Parser[Any] = nsTagChar ~ rep(nsUriChar)

  // Node Properties
  def cNsProperties(n: Int, c: Context):Parser[Any] = (cNsTagProperty ~ opt(sSeparate(n, c) ~ cNsAnchorProperty)) |
    (cNsAnchorProperty ~ opt(sSeparate(n, c) ~ cNsTagProperty))
    // Node Tags
  def cNsTagProperty:Parser[Any] = cVerbatimTag | cNsShorthandTag | cNonSpecificTag
  def cVerbatimTag:Parser[Any] = "!" ~ "<" ~ rep1(nsUriChar) ~ ">"
  def cNsShorthandTag:Parser[Any] = cTagHandle ~ rep1(nsTagChar)
  def cNonSpecificTag:Parser[Any] = "!"
    // Node Anchors
  def cNsAnchorProperty:Parser[Any] = "&" ~ nsAnchorName
  def nsAnchorChar:Parser[Any] = "^({,},(,),\\,)" <~ nsChar
  def nsAnchorName:Parser[Any] = rep1(nsAnchorChar)

  /*
   * Flow Styles
   */

  // Alias Nodes
  def cNsAliasNode:Parser[Any] = "*" ~ nsAnchorName

  // Empty Nodes
  def eScalar:Parser[Any] = "" // TODO
  def eNode:Parser[Any] = eScalar

  // Flow Scalar Styles
    // Double-Quoted Style
  def nbDoubleChar:Parser[Any] = cNsEscChar | ("^\"" ~> "^\\"  ~> nbJson)
  def nsDoubleChar:Parser[Any] = sWhite ~> nbDoubleChar // TODO: probably incorrect...
  def cDoubleQuoted(n: Int, c: Context):Parser[Any] = "\"" ~ nbDoubleText(n, c) ~ "\""
  def nbDoubleText(n: Int, c: Context):Parser[Any] = c match {
    case FlowOut  => nbDoubleMultiLine(n)
    case FlowIn   => nbDoubleMultiLine(n)
    case BlockKey => nbDoubleOneLine
    case FlowKey  => nbDoubleOneLine
  }
  def nbDoubleOneLine:Parser[Any] = rep(nbDoubleChar)
  def sDoubleEscaped(n: Int):Parser[Any] = rep(sWhite) ~ "\\" ~bNonContent ~ rep(lEmpty(n, FlowIn("flow-in"))) ~ sFlowFolded(n)
  def sDoubleBreak(n: Int):Parser[Any] = sDoubleEscaped(n) |  sFlowFolded(n)
  def nbNsDoubleInLine:Parser[Any] = rep(rep(sWhite) ~ nsDoubleChar)
  def sDoubleNextLine(n: Int):Parser[Any] = sDoubleBreak(n) ~ opt(nsDoubleChar ~ nbNsDoubleInLine ~ (sDoubleNextLine(n) | rep(sWhite)))
  def nbDoubleMultiLine(n: Int):Parser[Any] = nbNsDoubleInLine ~ (sDoubleNextLine(n) | rep(sWhite))
    // Single-Quoted Style
  def cQuotedQuote:Parser[Any] = "'" ~ "'"
  def nbSingleChar:Parser[Any] = cQuotedQuote | ("^'" ~> nbJson)
  def nsSingleChar:Parser[Any] = nbSingleChar ~> sWhite
  def cSingleQuoted(n: Int, c: Context):Parser[Any] = "'" ~ nbSingleText(n, c) ~ "'"
  def nbSingleText(n: Int, c: Context):Parser[Any] = c match {
    case FlowOut  => nbSingleMultiLine(n)
    case FlowIn   => nbSingleMultiLine(n)
    case BlockKey => nbSingleOneLine
    case FlowKey  => nbSingleOneLine
  }
  def nbSingleOneLine:Parser[Any] = rep(nbSingleChar)
  def nbNsSingleInLine:Parser[Any] = rep(rep(sWhite) ~ nsSingleChar)
  def sSingleNextLine(n: Int):Parser[Any] = sFlowFolded(n) ~ opt(nsSingleChar ~ nbNsSingleInLine ~(sSingleNextLine(n) | rep(sWhite)))
  def nbSingleMultiLine(n: Int):Parser[Any] = nbNsSingleInLine ~ (sSingleNextLine(n) | rep(sWhite))
    // Plain Style
  def nsPlainFirst(c: Context):Parser[Any] = (cIndicator ~> nsChar) | (("?" | ":" | "-") ~ nsPlainSafe(c))   // TODO: incorrect
  def nsPlainSafe(c: Context):Parser[Any] = c match {
    case FlowOut  => nsPlainSafeOut
    case FlowIn   => nsPlainSafeIn
    case BlockKey => nsPlainSafeOut
    case FlowKey  => nsPlainSafeIn
  }
  def nsPlainSafeOut:Parser[Any] = nsChar
  def nsPlainSafeIn:Parser[Any] = cFlowIndicator ~> nsChar  // TODO: incorrect
  def nsPlainChar(c: Context):Parser[Any] = ("^:" ~> "^#" ~> nsPlainSafe(c)) | (nsChar ~ "#") | (":" ~ nsPlainSafe(c))  // TODO: probably incorrect
  def nsPlain(n: Int, c: Context):Parser[Any] = c match {
    case FlowOut  => nsPlainMultiLine(n, c)
    case FlowIn   => nsPlainMultiLine(n, c)
    case BlockKey => nsPlainOneLine(c)
    case FlowKey  => nsPlainOneLine(c)
  }
  def nbNsPlainInLine(c: Context):Parser[Any] = rep(rep(sWhite) ~ nsPlainChar(c))
  def nsPlainOneLine(c: Context):Parser[Any] = nsPlainFirst(c) ~ nbNsPlainInLine(c)
  def sNsPlainNextLine(n: Int, c: Context):Parser[Any] = sFlowFolded(n) ~ nsPlainChar(c) ~ nbNsPlainInLine(c)
  def nsPlainMultiLine(n: Int, c: Context):Parser[Any] = nsPlainOneLine(c) ~ rep(sNsPlainNextLine(n, c))

  // Flow Collection Styles
  def inFlow(c: Context):Context = c match {
    case FlowOut  => FlowIn("flow-in")
    case FlowIn   => FlowIn("flow-in")
    case BlockKey => FlowKey("flow-key")
    case FlowKey  => FlowKey("flow-key")
  }
    // Flow Sequences
  def cFlowSequence(n: Int, c: Context):Parser[Any] = "[" ~ opt(sSeparate(n, c)) ~ opt(nsSFlowSeqEntries(n, inFlow(c))) ~ "]"
  def nsSFlowSeqEntries(n: Int, c: Context):Parser[Any] = nsFlowSeqEntry(n, c) ~
    opt(sSeparate(n, c)) ~
    opt("," ~ opt(sSeparate(n, c)) ~ opt(nsSFlowSeqEntries(n, c)))
  def nsFlowSeqEntry(n: Int, c: Context):Parser[Any] = nsFlowPair(n, c) | nsFlowNode(n, c)
    // Flow Mappings
  def cFlowMapping(n: Int, c: Context):Parser[Any] = "{" ~ opt(sSeparate(n, c)) ~ opt(nsSFlowMapEntries(n, inFlow(c))) ~ "}"
  def nsSFlowMapEntries(n: Int, c: Context):Parser[Any] = nsFlowMapEntry(n, c) ~ opt(sSeparate(n, c)) ~
    opt("," ~ opt(sSeparate(n, c)) ~ nsSFlowMapEntries(n, c))
  def nsFlowMapEntry(n: Int, c: Context):Parser[Any] = ("?" ~ sSeparate(n, c) ~ nsFlowMapExplicitEntry(n, c)) | nsFlowMapImplicitEntry(n, c)
  def nsFlowMapExplicitEntry(n: Int, c: Context):Parser[Any] = nsFlowMapImplicitEntry(n, c) | (eNode ~ eNode)
  def nsFlowMapImplicitEntry(n: Int, c: Context):Parser[Any] = nsFlowMapYamlKeyEntry(n, c) | cNsFlowMapEmptyKeyEntry(n, c) | cNsFlowMapJsonKeyEntry(n, c)
  def nsFlowMapYamlKeyEntry(n: Int, c: Context):Parser[Any] = nsFlowYamlNode(n, c) ~ (opt((sSeparate(n, c)) ~ cNsFlowMapSeparateValue(n, c)) | eNode)
  def cNsFlowMapEmptyKeyEntry(n: Int, c: Context):Parser[Any] = eNode ~ cNsFlowMapSeparateValue(n, c)
  def cNsFlowMapSeparateValue(n: Int, c: Context):Parser[Any] = ":" ~ (sSeparate(n, c) ~ nsFlowNode(n, c) | eNode)
  def cNsFlowMapJsonKeyEntry(n: Int, c: Context):Parser[Any] = cFlowJsonNode(n, c) ~ ((opt(sSeparate(n, c)) ~ cNsFlowMapAdjacentValue(n, c)) | eNode)
  def cNsFlowMapAdjacentValue(n: Int, c: Context):Parser[Any] = ":" ~((opt(sSeparate(n, c)) ~ nsFlowNode(n, c)) | eNode)
  def nsFlowPair(n: Int, c: Context):Parser[Any] = ("?" ~ sSeparate(n, c) ~ nsFlowMapExplicitEntry(n, c)) | nsFlowPairEntry(n, c)
  def nsFlowPairEntry(n: Int, c: Context):Parser[Any] = nsFlowPairYamlKeyEntry(n, c) | cNsFlowMapEmptyKeyEntry(n, c) | cNsFlowPairJsonKeyEntry(n, c)
  def nsFlowPairYamlKeyEntry(n: Int, c: Context):Parser[Any] = nsSImplicitYamlKey(FlowKey("flow-key")) ~ cNsFlowMapSeparateValue(n, c)
  def cNsFlowPairJsonKeyEntry(n: Int, c: Context):Parser[Any] = cSImplicitJsonKey(FlowKey("flow-key")) ~ cNsFlowMapAdjacentValue(n, c)
  def nsSImplicitYamlKey(c: Context):Parser[Any] = nsFlowYamlNode(1024, c) ~ sSeparateInLine? // TODO: 1024 ?
  def cSImplicitJsonKey(c: Context):Parser[Any] = cFlowJsonNode(1024, c) ~ sSeparateInLine? // TODO: 1024 ?
    // Flow Nodes
  def nsFlowYamlContent(n: Int, c: Context):Parser[Any] = nsPlain(n, c)
  def cFlowJsonContent(n: Int, c: Context):Parser[Any] = cFlowSequence(n, c) | cFlowMapping(n, c) | cSingleQuoted(n, c) | cDoubleQuoted(n, c)
  def nsFlowContent(n: Int, c: Context):Parser[Any] = nsFlowYamlContent(n, c) | cFlowJsonContent(n, c)
  def nsFlowYamlNode(n: Int, c: Context):Parser[Any] = cNsAliasNode | nsFlowYamlContent(n, c) |
    (cNsProperties(n, c) ~ ((sSeparate(n, c) ~ nsFlowYamlContent(n, c)) | eScalar))
  def cFlowJsonNode(n: Int, c: Context):Parser[Any] = opt(cNsProperties(n, c) ~ sSeparate(n, c)) ~ cFlowJsonContent(n, c)
  def nsFlowNode(n: Int, c: Context):Parser[Any] = cNsAliasNode | nsFlowContent(n, c) |
    (cNsProperties(n, c) ~ ((sSeparate(n, c) ~ nsFlowContent(n, c)) | eScalar))

  /*
   * Block Styles
   */

  def cBBlockHeader(m:Int, t:Chomping):Parser[Any] = ((cIndentationIndicator ~ cChompingIndicator) |
    (cChompingIndicator ~ cIndentationIndicator)) ~ sBComment

  var m:Int
  var t:Chomping

  def cIndentationIndicator:Parser[Any] = (nsDecDigit | "") ^^ {x:String => x match { // TODO: may be...
    case "" => -1
    case _  => x.toInt
  }}

  def cChompingIndicator:Parser[Any] = "-" | "+" | "" ^^ {x:String => x match { // TODO: may be...
    case "-" => t = Strip("strip")
    case "+" => t = Keep("keep")
    case ""  => t = Clip("clip")
  }}

  def bChompedLast(t:Chomping):Parser[Any] = t match {
    case Strip => bNonContent
    case Clip  => bAsLineFeed
    case Keep  => bAsLineFeed
  }
  def lChompedEmpty(n:Int, t:Chomping):Parser[Any] = t match {
    case Strip => lStripEmpty(n)
    case Clip  => lStripEmpty(n)
    case Keep  => lKeepEmpty(n)
  }

  def lStripEmpty(n:Int):Parser[Any] = rep(sIndentEqualOrLessThan(n) ~ bNonContent) ~ lTrailComments(n)?
  def lKeepEmpty(n:Int):Parser[Any] = rep(lEmpty(n, BlockIn("block-in"))) ~ lTrailComments(n)?
  def lTrailComments(n:Int):Parser[Any] = sIndentLessThan(n) ~ cNbCommentText ~ bComment ~ rep(lComment)
    // Literal Styles
  def cLpLiteral(n:Int):Parser[Any] = "|" ~ cBBlockHeader(m, t) ~ lLiteralContent(n + m, t)
  def lNbLiteralText(n:Int):Parser[Any] = rep(lEmpty(n, BlockIn("block-in"))) ~ sIndent(n) ~ rep1(nbChar)
  def bNbLiteralNext(n:Int):Parser[Any] = bAsLineFeed ~ lNbLiteralText(n)
  def lLiteralContent(n:Int, t:Chomping):Parser[Any] = opt(lNbLiteralText(n) ~ rep(bNbLiteralNext(n)) ~ bChompedLast(t)) ~ lChompedEmpty(n, t)
    // Folded Style
  def cLFolded(n:Int):Parser[Any] = ">" ~ cBBlockHeader(m, t)








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

  case class Context(s:String)
  case class BlockOut(override val s: String) extends Context(s)
  case class BlockIn(override val s: String) extends Context(s)
  case class FlowOut(override val s: String) extends Context(s)
  case class FlowIn(override val s: String) extends Context(s)
  case class BlockKey(override val s:String) extends Context(s)
  case class FlowKey(override val s:String) extends Context(s)

  case class Chomping(s:String)
  case class Strip(override val s:String) extends Chomping(s)
  case class Keep(override val s:String) extends Chomping(s)
  case class Clip(override val s:String) extends Chomping(s)
  case class Unknown(override val s:String) extends Chomping(s)
}


