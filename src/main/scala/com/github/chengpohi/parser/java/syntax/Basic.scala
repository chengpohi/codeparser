package com.github.chengpohi.parser.java.syntax

import fastparse.all._

/**
  * codeparser
  * Created by chengpohi on 5/1/16.
  */
object Basic {
  val UnicodeEscape = P("u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit)

  val digits = "0123456789"
  val Digit = P(CharIn(digits))
  val hexDigits = digits + "abcdefABCDEF"
  val HexDigit = P(CharIn(hexDigits))
  val HexNum = P("0x" ~ CharsWhile(hexDigits.contains(_)))
  val DecNum = P(CharsWhile(digits.contains(_)))
  val Exp = P(CharIn("Ee") ~ CharIn("+-").? ~ DecNum)
  val FloatType = P(CharIn("fFdD"))

  //space and tab
  val WSChars = P(CharsWhile("\u0020\u0009".contains(_)))
  val Newline = P(StringIn("\r\n", "\n"))
  val Semi = P(";" | Newline.rep(1))
  val OpChar = P(CharPred(isOpChar))

  def isOpChar(c: Char) = {
    fastparse.CharPredicates.isOtherSymbol(c) || fastparse.CharPredicates.isMathSymbol(c) || "!%&*+-/:<=>?@\\^|~".contains(c)
  }

  val Letter = P(CharPred(c => c.isLetter | c.isDigit))
  val LetterDigitDollarUnderscore = P(CharPred(c => c.isLetter | c.isDigit))
  val Lower = P(CharPred(c => c.isLower))
  val Upper = P(CharPred(_.isUpper))
}

object Key {
  def W(s: String) = P( s ~ !Basic.LetterDigitDollarUnderscore )
  // If the operator is followed by a comment, stop early so we can parse the comment
  def O(s: String) = P( s ~ (!Basic.OpChar | &("/*" | "//")) )
}
