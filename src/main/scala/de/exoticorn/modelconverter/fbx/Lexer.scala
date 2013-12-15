package de.exoticorn.modelconverter.fbx

case class LexerError(str: String) extends Exception(str)

class Lexer(src: Iterator[Char]) {
  var currentChar: Char = 0

  def nextChar(): Char =
    if (currentChar != 0) {
      val c = currentChar
      currentChar = 0
      c
    } else src.next()

  def putBack(c: Char) {
    assert(currentChar == 0)
    currentChar = c
  }

  def eof = currentChar == 0 && !src.hasNext

  def nextToken(): Token =
    if (eof) Token.EOF
    else nextChar() match {
      case ';' =>
        skipComment()
        nextToken()

      case c if c <= 32 => nextToken()

      case c @ ('{' | '}' | ':' | ',') =>
        Token.Char(c)

      case '"' =>
        readString()

      case c if c == '-' || (c >= '0' && c <= '9') =>
        readNumber(c)

      case c if (c | 32) >= 'a' && (c | 32) <= 'z' =>
        readIdentifier(c)

      case c => throw LexerError(s"Unexpected char '$c'")
    }

  def skipComment() {
    if (!eof && nextChar() != 10) {
      skipComment()
    }
  }

  def readString(): Token = {
    val b = new StringBuilder()
    def loop() {
      val c = nextChar()
      if (c != '"') {
        b += c
        loop()
      }
    }
    loop()
    Token.String(b.result())
  }

  def readNumber(c: Char): Token.Number = {
    val b = new StringBuilder()
    b += c
    def loop(): Token.Number = {
      val c = nextChar()
      if (c == '.') {
        b += c
        loopDouble()
      } else if (c >= '0' && c <= '9') {
        b += c
        loop()
      } else {
        putBack(c)
        Token.Long(b.result().toLong)
      }
    }
    def loopDouble(): Token.Double = {
      val c = nextChar()
      if (c >= '0' && c <= '9') {
        b += c
        loopDouble()
      } else {
        putBack(c)
        Token.Double(b.result().toDouble)
      }
    }
    loop()
  }

  def readIdentifier(c: Char): Token.Identifier = {
    val b = new StringBuilder()
    b += c
    def loop() {
      val c = nextChar()
      if (((c | 32) >= 'a' && (c | 32) <= 'z') || (c >= '0' && c <= '9') || c == '_') {
        b += c
        loop()
      } else {
        putBack(c)
      }
    }
    loop()
    Token.Identifier(b.result())
  }
}

sealed trait Token

object Token {
  case class Identifier(id: java.lang.String) extends Token
  trait Number extends Token {
    def value: scala.Double
  }
  case class Long(v: scala.Long) extends Number {
    def value = v.toDouble
  }
  case class Double(val value: scala.Double) extends Number
  case class String(str: java.lang.String) extends Token
  case class Char(c: scala.Char) extends Token
  case object EOF extends Token
}