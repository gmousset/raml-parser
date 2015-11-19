package gmousset.yaml.parser.v1_2

import org.slf4j.LoggerFactory

import scala.io.Source
import scala.util.parsing.combinator._

/**
  * Created by Gwendal Mousset on 13/11/2015.
  */
class YamlParser(source: Source) extends RegexParsers {

  val logger = LoggerFactory.getLogger(this.getClass)
  var m:Int = _
  var t:Chomping = _


  /*
   * Characters
   */
  // Character Set
  // 1
  def cPrintable:Parser[String] = "\\u0009" | "\\u000A" | "\\u000D" | "[\\u0020-\\u007E]" | "\\u0085" |
    "[\\u00A0-\\uD7FF]" | "[\\uE000-\\uFFFD]" | "[\\u10000-\\u10FFFF]"
  // 2
  def nbJson:Parser[String] = "\\u0009" | "[\\u0020-\\u10FFFF]"

  // Character Encodings
  // 3
  def cByteOrderMask:Parser[String] = "\\uFEFF"

  // Indicator Characters
  // 4
  def cSequenceEntry:Parser[String] = "-"
  // 5
  def cMappingKey:Parser[String] = "?"
  // 6
  def cMappingValue:Parser[String] = ":"
  // 7
  def cCollectEntry:Parser[String] = ","
  // 8
  def cSequenceStart:Parser[String] = "["
  // 9
  def cSequenceEnd:Parser[String] = "]"
  // 10
  def cMappingStart:Parser[String] = "{"
  // 11
  def cMappingEnd:Parser[String] = "}"
  // 12
  def cComment:Parser[String] = "#"
  // 13
  def cAnchor:Parser[String] = "&"
  // 14
  def cAlias:Parser[String] = "*"
  // 15
  def cTag:Parser[String] = "!"
  // 16
  def cLiteral:Parser[String] = "|"
  // 17
  def cFolded:Parser[String] = ">"
  // 18
  def cSingleQuote:Parser[String] = "'"
  // 19
  def cDoubleQuote:Parser[String] = "\""
  // 20
  def cDirective:Parser[String] = "%"
  // 21
  def cReserved:Parser[String] = "@" | "`"
  // 22
  def cIndicator:Parser[String] = cSequenceEntry | cMappingKey | cMappingValue | cCollectEntry | cSequenceStart |
    cSequenceEnd | cMappingStart | cMappingEnd | cComment | cAnchor | cAlias | cTag | cLiteral | cFolded |
    cSingleQuote | cDoubleQuote | cDirective | cReserved
  // 23
  def cFlowIndicator:Parser[String] = cCollectEntry | cSequenceStart | cSequenceEnd | cMappingStart | cMappingEnd

  // Line Break Characters
  // 24
  def bLineFeed:Parser[String] = "\\u000A"
  // 25
  def bCarriageReturn:Parser[String] = "\\u000D"
  // 26
  def bChar:Parser[String] = bLineFeed | bCarriageReturn
  // 27
  def nbChar:Parser[String] = "\\u0009" | "[\\u0020-\\u007E]" | "\\u0085" |
    "[\\u00A0-\\uD7FF]" | "[\\uE000-\\uFFFD]" | "[\\u10000-\\u10FFFF]"  // TODO: order mask not removed
  // 28
  def bBreak:Parser[String] = bCarriageReturn | bLineFeed | bCarriageReturn | bLineFeed
  // 29
  def bAsLineFeed:Parser[String] = bBreak
  // 30
  def bNonContent:Parser[String] = bBreak

  // White Space Characters
  // 31
  def sSpace:Parser[String] = "\\u0020"
  // 32
  def sTab:Parser[String] = "\\u0009"
  // 33
  def sWhite:Parser[String] = sSpace | sTab
  // 34
  def nsChar:Parser[String] = "[\\u0021-\\u007E]" | "\\u0085" |
    "[\\u00A0-\\uD7FF]" | "[\\uE000-\\uFFFD]" | "[\\u10000-\\u10FFFF]"

  // Miscellaneous Characters
  // 35
  def nsDecDigit:Parser[String] = "[\\u0030-\\u0039]"
  // 36
  def nsHexDigit:Parser[String] = nsDecDigit | "[\\u0041-\\u0046]" | "[\\u0061-\\u0066]"
  // 37
  def nsAsciiLetter:Parser[String] = "[\\u0041-\\u005A]" | "[\\u0061-\\u007A]"
  // 38
  def nsWordChar:Parser[String] = nsDecDigit | nsAsciiLetter | "-"
  // 39
  def nsUriChar:Parser[Any] = "%" ~ nsHexDigit ~ nsHexDigit | nsWordChar | "#" | ";" | "/" | "?" | ":" | "@" | "&" |
    "=" | "+" | "$" | "," | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")" | "[" | "]"
  // 40
  def nsTagChar:Parser[Any] = "%" ~ nsHexDigit ~ nsHexDigit | nsWordChar | "#" | ";" | "/" | "?" | ":" | "@" | "&" |
    "=" | "+" | "$" | "_" | "." | "~" | "*" | "'" | "(" | ")"

  // Escaped Characters
  // 41
  def cEscape:Parser[String] = "\\"
  // 42
  def nsEscNull:Parser[String] = "0"
  // 43
  def nsEscBell:Parser[String] = "a"
  // 44
  def nsEscBackspace:Parser[String] = "b"
  // 45
  def nsEscHorizontalTab:Parser[String] = "t" | "\\u0009"
  // 46
  def nsEscLineFeed:Parser[String] = "n"
  // 47
  def nsEscVerticalTab:Parser[String] = "v"
  // 48
  def nsEscFormFeed:Parser[String] = "f"
  // 49
  def nsEscCarriageReturn:Parser[String] = "r"
  // 50
  def nsEscEscape:Parser[String] = "e"
  // 51
  def nsEscSpace:Parser[String] = "\\u0020"
  // 52
  def nsEscDoubleQuote:Parser[String] = """"""
  // 53
  def nsEscSlash:Parser[String] = "/"
  // 54
  def nsEscBackslash:Parser[String] = "\\"
  // 55
  def nsEscNextLine:Parser[String] = "N"
  // 56
  def nsEscNonBreakingSpace:Parser[String] = "_"
  // 57
  def nsEscLineSeparator:Parser[String] = "L"
  // 58
  def nsEscParagraphSeparator:Parser[String] = "P"
  // 59
  def nsEsc8Bit:Parser[String] = "x"
  // 60
  def nsEsc16Bit:Parser[String] = "u"
  // 61
  def nsEsc32Bit:Parser[String] = "U"
  // 62
  def cNsEscChar:Parser[Any] = "\\" ~ (nsEscNull | nsEscBell | nsEscBackspace | nsEscHorizontalTab | nsEscLineFeed |
    nsEscVerticalTab | nsEscFormFeed | nsEscCarriageReturn | nsEscEscape | nsEscSpace | nsEscDoubleQuote | nsEscSlash |
    nsEscBackslash | nsEscNextLine | nsEscNonBreakingSpace | nsEscLineSeparator | nsEscParagraphSeparator |
    nsEsc8Bit | nsEsc16Bit | nsEsc32Bit)


  /*
   * Basic Structures
   */

  // Indentation Spaces
  // 63
  def sIndent(n: Int):Parser[Any] = repN(n, sSpace)
  // 64
  def sIndentLessThan(n: Int):Parser[Any] = {
    rep(sSpace) match {
      case Success(list:List, _) if list.length < n => Parser {Failure("failure", _)}
      case _:ParseResult => _
    }
  }
  // 65
  def sIndentEqualOrLessThan(n: Int):Parser[Any] = {
    rep(sSpace) match {
      case Success(list:List, _) if list.length <= n => Parser {Failure("failure", _)}
      case _:ParseResult => _
    }
  }

  // Separation Spaces
  // 66
  def sSeparateInLine:Parser[Any] = rep1(sWhite)  // TODO:must add start of line

  // Line Prefixes
  // 67
  def sLinePrefix(n: Int, c:Context):Parser[Any] =  c match  {
    case BlockOut => sBlockLinePrefix(n)
    case BlockIn  => sBlockLinePrefix(n)
    case FlowOut  => sFlowLinePrefix(n)
    case FlowIn   => sFlowLinePrefix(n)
  }
  // 68
  def sBlockLinePrefix(n: Int):Parser[Any] = sIndent(n)
  // 69
  def sFlowLinePrefix(n: Int):Parser[Any] = sIndent(n) ~ opt(sSeparateInLine)

  // Empty Lines
  // 70
  def lEmpty(n: Int, c: Context):Parser[Any] = sLinePrefix(n, c) | sIndentLessThan(n) | bAsLineFeed

  // Line Folding
  // 71
  def bLTrimmed(n: Int, c: Context):Parser[Any] = bNonContent ~ lEmpty(n, c)
  // 72
  def bAsSpace:Parser[Any] = bBreak
  // 73
  def bLFolded(n: Int, c: Context):Parser[Any] = bLTrimmed(n, c) | bAsSpace
  // 74
  def sFlowFolded(n: Int):Parser[Any] = opt(sSeparateInLine) ~ bLFolded(n, FlowIn("flow-in")) ~ sFlowLinePrefix(n)


  // Comments
  // 75
  def cNbCommentText:Parser[Any] = "#" ~ rep(nbChar)
  // 76
  def bComment:Parser[Any] = bNonContent // TODO: must add end of file
  // 77
  def sBComment:Parser[Any] = opt(sSeparateInLine ~ opt(cNbCommentText)) ~ bComment
  // 78
  def lComment:Parser[Any] = sSeparateInLine ~ opt(cNbCommentText) ~ bComment
  // 79
  def sLComments:Parser[Any] = (sBComment /* TODO: must add start of line */) ~ rep(sLComments)

  // Separation Lines
  // 80
  def sSeparate(n: Int, c: Context):Parser[Any] = c match {
    case BlockOut => sSeparateLines(n)
    case BlockIn  => sSeparateLines(n)
    case FlowOut  => sSeparateLines(n)
    case FlowIn   => sSeparateLines(n)
    case BlockKey => sSeparateInLine
    case FlowKey  => sSeparateInLine
  }
  // 81
  def sSeparateLines(n: Int):Parser[Any] = (sLComments ~ sFlowLinePrefix(n)) | sSeparateInLine

  // Directives
  // 82
  def lDirective:Parser[Any] = "%" ~ (nsYamlDirective | nsTagDirective | nsReservedDirective) ~ sLComments
  // 83
  def nsReservedDirective:Parser[Any] = nsDirectiveName ~ rep(sSeparateInLine ~ nsDirectiveParameter)
  // 84
  def nsDirectiveName:Parser[Any] = rep1(nsChar)
  // 85
  def nsDirectiveParameter:Parser[Any] = rep1(nsChar)

    // YAML Directives
  // 86
  def nsYamlDirective:Parser[Any] = "Y" ~ "A" ~ "M" ~ "L" ~ sSeparateInLine ~ nsYamlVersion
  // 87
  def nsYamlVersion:Parser[Any] = rep1(nsDecDigit) ~ "." ~ rep1(nsDecDigit)
    // Tag Directives
  // 88
  def nsTagDirective:Parser[Any] = "T" ~ "A" ~ "G" ~ sSeparateInLine ~ cTagHandle ~ sSeparateInLine ~ nsTagPrefix
    // Tag Handles
  // 89
  def cTagHandle:Parser[Any] = cNamedTagHandle | cSecondaryTagHandle | cPrimaryTagHandle
  // 90
  def cPrimaryTagHandle:Parser[Any] = "!"
  // 91
  def cSecondaryTagHandle:Parser[Any] = "!" ~ "!"
  // 92
  def cNamedTagHandle:Parser[Any] = "!" ~ rep1(nsWordChar) ~ "!"
    // Tag Prefixes
  // 93
  def nsTagPrefix:Parser[Any] = cNsLocalTagPrefix | nsGlobalTagPrefix
  // 94
  def cNsLocalTagPrefix:Parser[Any] = "!" ~ rep(nsUriChar)
  // 95
  def nsGlobalTagPrefix:Parser[Any] = nsTagChar ~ rep(nsUriChar)

  // Node Properties
  // 96
  def cNsProperties(n: Int, c: Context):Parser[Any] = (cNsTagProperty ~ opt(sSeparate(n, c) ~ cNsAnchorProperty)) |
    (cNsAnchorProperty ~ opt(sSeparate(n, c) ~ cNsTagProperty))
    // Node Tags
  // 97
  def cNsTagProperty:Parser[Any] = cVerbatimTag | cNsShorthandTag | cNonSpecificTag
  // 98
  def cVerbatimTag:Parser[Any] = "!" ~ "<" ~ rep1(nsUriChar) ~ ">"
  // 99
  def cNsShorthandTag:Parser[Any] = cTagHandle ~ rep1(nsTagChar)
  // 100
  def cNonSpecificTag:Parser[Any] = "!"
    // Node Anchors
  // 101
  def cNsAnchorProperty:Parser[Any] = "&" ~ nsAnchorName
  // 102
  def nsAnchorChar:Parser[Any] = "^({,},(,),\\,)" <~ nsChar
  // 103
  def nsAnchorName:Parser[Any] = rep1(nsAnchorChar)

  /*
   * Flow Styles
   */

  // Alias Nodes
  // 104
  def cNsAliasNode:Parser[Any] = "*" ~ nsAnchorName

  // Empty Nodes
  // 105
  def eScalar:Parser[Any] = "" // TODO
  // 106
  def eNode:Parser[Any] = eScalar

  // Flow Scalar Styles
    // Double-Quoted Style
  // 107
  def nbDoubleChar:Parser[Any] = cNsEscChar | ("^\"" ~> "^\\"  ~> nbJson)
  // 108
  def nsDoubleChar:Parser[Any] = sWhite ~> nbDoubleChar // TODO: probably incorrect...
  // 109
  def cDoubleQuoted(n: Int, c: Context):Parser[Any] = "\"" ~ nbDoubleText(n, c) ~ "\""
  // 110
  def nbDoubleText(n: Int, c: Context):Parser[Any] = c match {
    case FlowOut  => nbDoubleMultiLine(n)
    case FlowIn   => nbDoubleMultiLine(n)
    case BlockKey => nbDoubleOneLine
    case FlowKey  => nbDoubleOneLine
  }
  // 111
  def nbDoubleOneLine:Parser[Any] = rep(nbDoubleChar)
  // 112
  def sDoubleEscaped(n: Int):Parser[Any] = rep(sWhite) ~ "\\" ~bNonContent ~ rep(lEmpty(n, FlowIn("flow-in"))) ~ sFlowFolded(n)
  // 113
  def sDoubleBreak(n: Int):Parser[Any] = sDoubleEscaped(n) |  sFlowFolded(n)
  // 114
  def nbNsDoubleInLine:Parser[Any] = rep(rep(sWhite) ~ nsDoubleChar)
  // 115
  def sDoubleNextLine(n: Int):Parser[Any] = sDoubleBreak(n) ~ opt(nsDoubleChar ~ nbNsDoubleInLine ~ (sDoubleNextLine(n) | rep(sWhite)))
  // 116
  def nbDoubleMultiLine(n: Int):Parser[Any] = nbNsDoubleInLine ~ (sDoubleNextLine(n) | rep(sWhite))
    // Single-Quoted Style
  // 117
  def cQuotedQuote:Parser[Any] = "'" ~ "'"
  // 118
  def nbSingleChar:Parser[Any] = cQuotedQuote | ("^'" ~> nbJson)
  // 119
  def nsSingleChar:Parser[Any] = nbSingleChar ~> sWhite
  // 120
  def cSingleQuoted(n: Int, c: Context):Parser[Any] = "'" ~ nbSingleText(n, c) ~ "'"
  // 121
  def nbSingleText(n: Int, c: Context):Parser[Any] = c match {
    case FlowOut  => nbSingleMultiLine(n)
    case FlowIn   => nbSingleMultiLine(n)
    case BlockKey => nbSingleOneLine
    case FlowKey  => nbSingleOneLine
  }
  // 122
  def nbSingleOneLine:Parser[Any] = rep(nbSingleChar)
  // 123
  def nbNsSingleInLine:Parser[Any] = rep(rep(sWhite) ~ nsSingleChar)
  // 124
  def sSingleNextLine(n: Int):Parser[Any] = sFlowFolded(n) ~ opt(nsSingleChar ~ nbNsSingleInLine ~(sSingleNextLine(n) | rep(sWhite)))
  // 125
  def nbSingleMultiLine(n: Int):Parser[Any] = nbNsSingleInLine ~ (sSingleNextLine(n) | rep(sWhite))
    // Plain Style
  // 126
  def nsPlainFirst(c: Context):Parser[Any] = (cIndicator ~> nsChar) | (("?" | ":" | "-") ~ nsPlainSafe(c))   // TODO: incorrect
  // 127
  def nsPlainSafe(c: Context):Parser[Any] = c match {
    case FlowOut  => nsPlainSafeOut
    case FlowIn   => nsPlainSafeIn
    case BlockKey => nsPlainSafeOut
    case FlowKey  => nsPlainSafeIn
  }
  // 128
  def nsPlainSafeOut:Parser[Any] = nsChar
  // 129
  def nsPlainSafeIn:Parser[Any] = cFlowIndicator ~> nsChar  // TODO: incorrect
  // 130
  def nsPlainChar(c: Context):Parser[Any] = ("^:" ~> "^#" ~> nsPlainSafe(c)) | (nsChar ~ "#") | (":" ~ nsPlainSafe(c))  // TODO: probably incorrect
  // 131
  def nsPlain(n: Int, c: Context):Parser[Any] = c match {
    case FlowOut  => nsPlainMultiLine(n, c)
    case FlowIn   => nsPlainMultiLine(n, c)
    case BlockKey => nsPlainOneLine(c)
    case FlowKey  => nsPlainOneLine(c)
  }
  // 132
  def nbNsPlainInLine(c: Context):Parser[Any] = rep(rep(sWhite) ~ nsPlainChar(c))
  // 133
  def nsPlainOneLine(c: Context):Parser[Any] = nsPlainFirst(c) ~ nbNsPlainInLine(c)
  // 134
  def sNsPlainNextLine(n: Int, c: Context):Parser[Any] = sFlowFolded(n) ~ nsPlainChar(c) ~ nbNsPlainInLine(c)
  // 135
  def nsPlainMultiLine(n: Int, c: Context):Parser[Any] = nsPlainOneLine(c) ~ rep(sNsPlainNextLine(n, c))

  // Flow Collection Styles
  // 136
  def inFlow(c: Context):Context = c match {
    case FlowOut  => FlowIn("flow-in")
    case FlowIn   => FlowIn("flow-in")
    case BlockKey => FlowKey("flow-key")
    case FlowKey  => FlowKey("flow-key")
  }
    // Flow Sequences
  // 137
  def cFlowSequence(n: Int, c: Context):Parser[Any] = "[" ~ opt(sSeparate(n, c)) ~ opt(nsSFlowSeqEntries(n, inFlow(c))) ~ "]"
  // 138
  def nsSFlowSeqEntries(n: Int, c: Context):Parser[Any] = nsFlowSeqEntry(n, c) ~
    opt(sSeparate(n, c)) ~
    opt("," ~ opt(sSeparate(n, c)) ~ opt(nsSFlowSeqEntries(n, c)))
  // 139
  def nsFlowSeqEntry(n: Int, c: Context):Parser[Any] = nsFlowPair(n, c) | nsFlowNode(n, c)
    // Flow Mappings
  // 140
  def cFlowMapping(n: Int, c: Context):Parser[Any] = "{" ~ opt(sSeparate(n, c)) ~ opt(nsSFlowMapEntries(n, inFlow(c))) ~ "}"
  // 141
  def nsSFlowMapEntries(n: Int, c: Context):Parser[Any] = nsFlowMapEntry(n, c) ~ opt(sSeparate(n, c)) ~
    opt("," ~ opt(sSeparate(n, c)) ~ nsSFlowMapEntries(n, c))
  // 142
  def nsFlowMapEntry(n: Int, c: Context):Parser[Any] = ("?" ~ sSeparate(n, c) ~ nsFlowMapExplicitEntry(n, c)) | nsFlowMapImplicitEntry(n, c)
  // 143
  def nsFlowMapExplicitEntry(n: Int, c: Context):Parser[Any] = nsFlowMapImplicitEntry(n, c) | (eNode ~ eNode)
  // 144
  def nsFlowMapImplicitEntry(n: Int, c: Context):Parser[Any] = nsFlowMapYamlKeyEntry(n, c) | cNsFlowMapEmptyKeyEntry(n, c) | cNsFlowMapJsonKeyEntry(n, c)
  // 145
  def nsFlowMapYamlKeyEntry(n: Int, c: Context):Parser[Any] = nsFlowYamlNode(n, c) ~ (opt((sSeparate(n, c)) ~ cNsFlowMapSeparateValue(n, c)) | eNode)
  // 146
  def cNsFlowMapEmptyKeyEntry(n: Int, c: Context):Parser[Any] = eNode ~ cNsFlowMapSeparateValue(n, c)
  // 147
  def cNsFlowMapSeparateValue(n: Int, c: Context):Parser[Any] = ":" ~ (sSeparate(n, c) ~ nsFlowNode(n, c) | eNode)
  // 148
  def cNsFlowMapJsonKeyEntry(n: Int, c: Context):Parser[Any] = cFlowJsonNode(n, c) ~ ((opt(sSeparate(n, c)) ~ cNsFlowMapAdjacentValue(n, c)) | eNode)
  // 149
  def cNsFlowMapAdjacentValue(n: Int, c: Context):Parser[Any] = ":" ~((opt(sSeparate(n, c)) ~ nsFlowNode(n, c)) | eNode)
  // 150
  def nsFlowPair(n: Int, c: Context):Parser[Any] = ("?" ~ sSeparate(n, c) ~ nsFlowMapExplicitEntry(n, c)) | nsFlowPairEntry(n, c)
  // 151
  def nsFlowPairEntry(n: Int, c: Context):Parser[Any] = nsFlowPairYamlKeyEntry(n, c) | cNsFlowMapEmptyKeyEntry(n, c) | cNsFlowPairJsonKeyEntry(n, c)
  // 152
  def nsFlowPairYamlKeyEntry(n: Int, c: Context):Parser[Any] = nsSImplicitYamlKey(FlowKey("flow-key")) ~ cNsFlowMapSeparateValue(n, c)
  // 153
  def cNsFlowPairJsonKeyEntry(n: Int, c: Context):Parser[Any] = cSImplicitJsonKey(FlowKey("flow-key")) ~ cNsFlowMapAdjacentValue(n, c)
  // 154
  def nsSImplicitYamlKey(c: Context):Parser[Any] = nsFlowYamlNode(1024, c) ~ sSeparateInLine? // TODO: 1024 ?
  // 155
  def cSImplicitJsonKey(c: Context):Parser[Any] = cFlowJsonNode(1024, c) ~ sSeparateInLine? // TODO: 1024 ?
    // Flow Nodes
  // 156
  def nsFlowYamlContent(n: Int, c: Context):Parser[Any] = nsPlain(n, c)
  // 157
  def cFlowJsonContent(n: Int, c: Context):Parser[Any] = cFlowSequence(n, c) | cFlowMapping(n, c) | cSingleQuoted(n, c) | cDoubleQuoted(n, c)
  // 158
  def nsFlowContent(n: Int, c: Context):Parser[Any] = nsFlowYamlContent(n, c) | cFlowJsonContent(n, c)
  // 159
  def nsFlowYamlNode(n: Int, c: Context):Parser[Any] = cNsAliasNode | nsFlowYamlContent(n, c) |
    (cNsProperties(n, c) ~ ((sSeparate(n, c) ~ nsFlowYamlContent(n, c)) | eScalar))
  // 160
  def cFlowJsonNode(n: Int, c: Context):Parser[Any] = opt(cNsProperties(n, c) ~ sSeparate(n, c)) ~ cFlowJsonContent(n, c)
  // 161
  def nsFlowNode(n: Int, c: Context):Parser[Any] = cNsAliasNode | nsFlowContent(n, c) |
    (cNsProperties(n, c) ~ ((sSeparate(n, c) ~ nsFlowContent(n, c)) | eScalar))

  /*
   * Block Styles
   */
  // 162
  def cBBlockHeader(m:Int, t:Chomping):Parser[Any] = ((cIndentationIndicator ~ cChompingIndicator) |
    (cChompingIndicator ~ cIndentationIndicator)) ~ sBComment

  // 163
  def cIndentationIndicator:Parser[Any] = (nsDecDigit | "") ^^ {x:String => x match { // TODO: may be...
    case "" => -1
    case _  => x.toInt
  }}
  // 164
  def cChompingIndicator:Parser[Any] = "-" | "+" | "" ^^ {x:String => x match { // TODO: may be...
    case "-" => t = Strip("strip")
    case "+" => t = Keep("keep")
    case ""  => t = Clip("clip")
  }}
  // 165
  def bChompedLast(t:Chomping):Parser[Any] = t match {
    case Strip => bNonContent
    case Clip  => bAsLineFeed
    case Keep  => bAsLineFeed
  }
  // 166
  def lChompedEmpty(n:Int, t:Chomping):Parser[Any] = t match {
    case Strip => lStripEmpty(n)
    case Clip  => lStripEmpty(n)
    case Keep  => lKeepEmpty(n)
  }
  // 167
  def lStripEmpty(n:Int):Parser[Any] = rep(sIndentEqualOrLessThan(n) ~ bNonContent) ~ lTrailComments(n)?
  // 168
  def lKeepEmpty(n:Int):Parser[Any] = rep(lEmpty(n, BlockIn("block-in"))) ~ lTrailComments(n)?
  // 169
  def lTrailComments(n:Int):Parser[Any] = sIndentLessThan(n) ~ cNbCommentText ~ bComment ~ rep(lComment)
    // Literal Styles
  // 170
  def cLpLiteral(n:Int):Parser[Any] = "|" ~ cBBlockHeader(m, t) ~ lLiteralContent(n + m, t)
  // 171
  def lNbLiteralText(n:Int):Parser[Any] = rep(lEmpty(n, BlockIn("block-in"))) ~ sIndent(n) ~ rep1(nbChar)
  // 172
  def bNbLiteralNext(n:Int):Parser[Any] = bAsLineFeed ~ lNbLiteralText(n)
  // 173
  def lLiteralContent(n:Int, t:Chomping):Parser[Any] = opt(lNbLiteralText(n) ~ rep(bNbLiteralNext(n)) ~ bChompedLast(t)) ~ lChompedEmpty(n, t)
    // Folded Style
  // 174
  def cLpFolded(n:Int):Parser[Any] = ">" ~ cBBlockHeader(m, t) ~ lFoldedContent(n + m, t)
  // 175
  def sNbFoldedText(n:Int):Parser[Any] = sIndent(n) ~ nsChar ~ rep(nbChar)
  // 176
  def lNbFoldedLines(n:Int):Parser[Any] = sNbFoldedText(n) ~ rep(bLFolded(n, BlockIn("block-in")) ~ sNbFoldedText(n))
  // 177
  def sNbSpacedText(n:Int):Parser[Any] = sIndent(n) ~ sWhite ~ rep(nbChar)
  // 178
  def bLSpaced(n:Int):Parser[Any] = bAsLineFeed ~ lEmpty(n, BlockIn("block-in"))
  // 179
  def lNbSpacedLines(n:Int):Parser[Any] = sNbSpacedText(n) ~ rep(bLSpaced(n) ~ sNbSpacedText(n))
  // 180
  def lNbSameLines(n:Int):Parser[Any] = rep(lEmpty(n, BlockIn("block-in"))) ~ (lNbFoldedLines(n) | lNbSpacedLines(n))
  // 181
  def lNbDiffLines(n:Int):Parser[Any] = lNbSameLines(n) ~ rep(bAsLineFeed ~ lNbSameLines(n))
  // 182
  def lFoldedContent(n:Int, t:Chomping):Parser[Any] = opt(lNbDiffLines(n) ~ bChompedLast(t)) ~ lChompedEmpty(n, t)

  // Block Collection Styles
    // Block Sequences
  // 183
  def lpBlockSequence(n:Int):Parser[Any] = rep1(sIndent(n + m) ~ cLBlockSeqEntry(n + m))
  // 184
  def cLBlockSeqEntry(n:Int):Parser[Any] = "-" ~ sLpBlockIntented(n, BlockIn("block-in"))
  // 185
  def sLpBlockIntented(n:Int, c:Context):Parser[Any] = (sIndent(m) ~ (nsLCompactSequence(n + 1 + m) | nsLCompactMapping(n + 1 + m))) |
    sLpBlockNode(n, c) |
    (eNode ~ sLComments)
  // 186
  def nsLCompactSequence(n:Int):Parser[Any] = cLBlockSeqEntry(n) ~ rep(sIndent(n) ~ cLBlockSeqEntry(n))
    // Block Mappings
  // 187
  def lpBlockMapping(n:Int):Parser[Any] = rep1(sIndent(n + m) ~ nsLBlockMapEntry(n + m))
  // 188
  def nsLBlockMapEntry(n:Int):Parser[Any] = cLBlockMapExplicitEntry(n) | nsLBlockMapImplicitEntry(n)
  // 189
  def cLBlockMapExplicitEntry(n:Int):Parser[Any] = cLBlockMapExplicitKey(n) ~ (lBlockMapExplicitValue(n) | eNode)
  // 190
  def cLBlockMapExplicitKey(n:Int):Parser[Any] = "?" ~ sLpBlockIntented(n, BlockOut("block-out"))
  // 191
  def lBlockMapExplicitValue(n:Int):Parser[Any] = sIndent(n) ~ ":" ~ sLpBlockIntented(n, BlockOut("block-out"))
  // 192
  def nsLBlockMapImplicitEntry(n:Int):Parser[Any] = (nsSBlockMapImplicitKey | eNode) ~ cLBlockMapImplicitValue(n)
  // 193
  def nsSBlockMapImplicitKey:Parser[Any] = cSImplicitJsonKey(BlockKey("block-key")) | nsSImplicitYamlKey(BlockKey("block-key"))
  // 194
  def cLBlockMapImplicitValue(n:Int):Parser[Any] = ":" ~ (sLpBlockNode(n, BlockOut("block-out")) | (eNode ~ sLComments))
  // 195
  def nsLCompactMapping(n:Int):Parser[Any] = nsLBlockMapEntry(n) ~ rep(sIndent(n) ~ nsLBlockMapEntry(n))
    // Block Nodes
  // 196
  def sLpBlockNode(n:Int, c:Context):Parser[Any] = sLpBlockInBlock(n, c) | sLFlowInBlock(n)
  // 197
  def sLFlowInBlock(n:Int):Parser[Any] = sSeparate(n + 1, FlowOut("flow-out")) ~ nsFlowNode(n + 1, FlowOut("flow-out")) ~ sLComments
  // 198
  def sLpBlockInBlock(n:Int, c:Context):Parser[Any] = sLpBlockScalar(n, c) | sLpBlockCollection(n, c)
  // 199
  def sLpBlockScalar(n:Int, c:Context):Parser[Any] = sSeparate(n + 1, c) ~ opt(cNsProperties(n + 1, c) ~ sSeparate(n + 1, c)) ~ (cLpLiteral(n) | cLpFolded(n))
  // 200
  def sLpBlockCollection(n:Int, c:Context):Parser[Any] = opt(sSeparate(n + 1, c) ~ cNsProperties(n + 1, c)) ~ sLComments ~ (lpBlockSequence(seqSpace(n, c)) | lpBlockMapping(n))
  // 201
  def seqSpace(n:Int, c:Context):Int = c match {
    case BlockOut => n - 1
    case BlockIn  => n
  }




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


