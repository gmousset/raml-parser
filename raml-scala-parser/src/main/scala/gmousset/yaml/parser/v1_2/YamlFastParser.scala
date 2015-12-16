package gmousset.yaml.parser.v1_2

import fastparse.all._

/**
  * Created by Gwendal Mousset on 16/12/2015.
  */
class YamlFastParser {

  /*
   * CHARACTER SET
   */
  val c_printable = P(0x9.toChar.toString() | 0xA.toChar.toString() | 0xD.toChar.toString() | CharIn(0x20.toChar to 0x7E.toChar) | 0x85.toChar.toString() |
    CharIn(0xA0.toChar to 0xD7FF.toChar) | CharIn(0xE000.toChar to 0xFFFD.toChar) | CharIn(0x10000.toChar to 0x10FFFF.toChar))

  val nb_json = P(0x0009.toChar.toString() | CharIn(0x20.toChar to 0x10FFFF.toChar))

  /*
   * CHARACTER ENCODINGS
   */
  val c_byte_order_mark = P(0xFEFF.toChar.toString())

  /*
   * INDICATOR CHARACTERS
   */
  val c_sequence_entry = P("-")
  val c_mapping_key = P("?")
  val c_mapping_value = P(":")
  val c_collect_entry = P(",")
  val c_sequence_start = P("[")
  val c_sequence_end = P("]")
  val c_mapping_start = P("{")
  val c_mapping_end = P("}")
  val c_comment = P("#")
  val c_anchor = P("&")
  val c_alias = P("*")
  val c_tag = P("!")
  val c_litteral = P("|")
  val c_folded = P(">")
  val c_single_quote = P("'")
  val c_double_quote = P("\"")
  val c_directive = P("%")
  val c_reserved = P("@" | "`")
  val c_indicator = P("-" | "?" | ":" | "," | "[" | "]" | "{" | "}" | "#" | "&" | "*" | "!" | "|" | ">" | "'" | "\"" | "%" | "@" | "`")
  val c_flow_indicator = P("," | "[" | "]" | "{" | "}")

  /*
   * LINE BREAK CHARACTERS
   */
  val b_line_feed = P(0xA.toChar.toString())
  val b_carriage_return = P(0xD.toChar.toString())
  val b_char = P(b_line_feed | b_carriage_return)
  val nb_char = P(0x9.toChar.toString() | CharIn(0x20.toChar to 0x7E.toChar) | 0x85.toChar.toString() |
    CharIn(0xA0.toChar to 0xD7FF.toChar) | CharIn(0xE000.toChar to 0xFFFD.toChar) | CharIn(0x10000.toChar to 0x10FFFF.toChar))
  val b_break = P((b_carriage_return ~ b_line_feed) | b_carriage_return | b_line_feed)
  val b_as_line_feed = P(b_break)
  val b_non_content = P(b_break)

  /*
   * WHITE SPACE CHARACTERS
   */
  val s_space = P(0x20.toChar.toString())
  val s_tab = P(0x9.toChar.toString())
  val s_white = P(s_space | s_tab)
  val ns_char = P(0x85.toChar.toString() | CharIn(0xA0.toChar to 0xD7FF.toChar) | CharIn(0xE000.toChar to 0xFFFD.toChar) | CharIn(0x10000.toChar to 0x10FFFF.toChar))

  /*
   * MISCELLANEOUS CHARACTERS
   */
  val ns_dec_digit = P(CharIn(0x30.toChar to 0x39.toChar))
  val ns_hex_digit = P(ns_dec_digit | CharIn(0x41.toChar to 0x46.toChar) | CharIn(0x61.toChar to 0x66.toChar))
  val ns_ascii_digit = P(CharIn(0x41.toChar to 0x5A.toChar) | CharIn(0x61.toChar to 0x7A.toChar))
  val ns_word_char = P(ns_dec_digit | ns_ascii_digit | "-")
  val ns_uri_char = P("%" ~ ns_hex_digit ~ ns_hex_digit | ns_word_char |
    "#" | ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | "," | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")" | "[" | "]")
  val ns_tag_char = P("%" ~ ns_hex_digit ~ ns_hex_digit | ns_word_char |
    "#" | ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | "_" | "." | "~" | "*" | "'" | "(" | ")")

  /*
   * ESCAPED CHARACTERS
   */
  val c_escape = P("\\")
  val ns_esc_null = P("0")
  val ns_esc_bell = P("a")
  val ns_esc_backspace = P("b")
  val ns_esc_horizontal_tab = P("t" | 0x9.toChar.toString())
  val ns_esc_line_feed = P("n")
  val ns_esc_vertical_tab = P("v")
  val ns_esc_form_feed = P("f")
  val ns_esc_carriage_return = P("r")
  val ns_esc_escape = P("e")
  val ns_esc_space = P(0x20.toChar.toString())
  val ns_esc_double_quote = P("\"")
  val ns_esc_slash = P("/")
  val ns_esc_backslash = P("\\")
  val ns_esc_next_line = P("N")
  val ns_esc_non_breaking_space = P("_")
  val ns_esc_line_separator = P("L")
  val ns_esc_paragraph_separator = P("P")
  val ns_esc_8_bit = P("x" ~ ns_hex_digit.rep(2))
  val ns_esc_16_bit = P("u" ~ ns_hex_digit.rep(4))
  val ns_esc_32_bit = P("U" ~ ns_hex_digit.rep(8))
  val c_ns_esc_char = P("\\" ~ (ns_esc_null | ns_esc_bell | ns_esc_backspace | ns_esc_horizontal_tab | ns_esc_line_feed |
    ns_esc_vertical_tab | ns_esc_form_feed | ns_esc_carriage_return | ns_esc_escape | ns_esc_space | ns_esc_double_quote |
    ns_esc_slash | ns_esc_backslash | ns_esc_next_line | ns_esc_non_breaking_space | ns_esc_line_separator |
    ns_esc_paragraph_separator | ns_esc_8_bit | ns_esc_16_bit | ns_esc_32_bit))


  /*
   * INDENTATION SPACES
   */
  def s_indent(n:Int) = P(s_space.rep(n))
  def s_indentLessThan(n:Int, m:Int) = P(s_space.rep(m))
  def s_indentEqualOrLessThan(n:Int, m:Int) = P(s_space.rep(m))

  /*
   * SEPARATION SPACES
   */
  val s_separate_in_line = P(s_white.rep(1) /* | TODO: START OF LINE */)



  val letter = P(CharIn(0x20.toChar to 0x7E.toChar))
  val digit = P(CharIn('0' to '9'))
  val word = P(letter.rep(2) ~ digit.rep(2))

  def parseTest = {
    println("START")
    val parsed1 = c_printable.parse("a")
    println(">" + parsed1)
    println("END")
  }

}
