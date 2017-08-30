package rpm4s

package object utils {

  def isAlpha(c: Char): Boolean = {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  }

  def isNum(c: Char): Boolean = {
    c >= '0' && c <= '9'
  }

  def isAlphaNumOr(c: Char, validChars: String): Boolean = {
    isAlpha(c) || isNum(c) || validChars.contains(c)
  }

}
