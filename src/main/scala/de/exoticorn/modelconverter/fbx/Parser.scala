package de.exoticorn.modelconverter.fbx

import scala.collection.immutable._

case class ParseError(str: String) extends Exception(str)

object Parser {
  private class Tokens(lexer: Lexer) {
    private var current: Token = null
    def isEof = peek() == Token.EOF
    def isIdentifier = peek().isInstanceOf[Token.Identifier]
    def peek(): Token = {
      if (current == null) {
        current = lexer.nextToken()
      }
      current
    }
    def next() = {
      val t = peek()
      current = null
      t
    }
    def putBack(token: Token) {
      assert(current == null)
      current = token
    }
    def nextIf(t: Token): Boolean = {
      if (peek() == t) {
        next()
        true
      } else false
    }
    def expect(t: Token) {
      val f = next()
      if (f != t) {
        throw new ParseError(s"Expected '$t' but found '$f'")
      }
    }
    def expectChar(c: Char) {
      expect(Token.Char(c))
    }
    def expectIdentifier(): String = {
      next() match {
        case Token.Identifier(id) => id
        case t => throw new ParseError(s"Expected identifier but found '$t'")
      }
    }
  }

  def parse(lexer: Lexer): FbxNode = {
    val tokens = new Tokens(lexer)

    def parseNodes(): Seq[FbxNode] = {
      val builder = scala.collection.mutable.ListBuffer.empty[FbxNode]
      while (tokens.isIdentifier) {
        val tpe = tokens.expectIdentifier()
        tokens.expectChar(':')
        val attributes = parseAttributes()
        val children = if (tokens.nextIf(Token.Char('{'))) {
          val nodes = parseNodes()
          tokens.expectChar('}')
          nodes
        } else Seq.empty[FbxNode]

        builder += FbxNode(tpe, attributes, children)
      }
      builder.result()
    }

    def parseAttributes(): FbxArray = {
      def parseLongArray(): FbxArray = {
        val builder = scala.collection.mutable.ArrayBuilder.make[Long]
        def loop(): FbxArray = {
          tokens.next() match {
            case Token.Long(v) =>
              builder += v
              if (tokens.nextIf(Token.Char(',')))
                loop()
              else FbxLongArray(builder.result())
            case t =>
              tokens.putBack(t)
              parseValueArray(builder.result() map (v => FbxLong(v)))
          }
        }
        loop()
      }

      def parseDoubleArray(): FbxArray = {
        val builder = scala.collection.mutable.ArrayBuilder.make[Double]
        def loop(): FbxArray = {
          tokens.next() match {
            case Token.Double(v) =>
              builder += v
              if (tokens.nextIf(Token.Char(',')))
                loop()
              else FbxDoubleArray(builder.result())
            case t =>
              tokens.putBack(t)
              parseValueArray(builder.result() map (v => FbxDouble(v)))
          }
        }
        loop()
      }

      def parseValueArray(elems: Array[FbxValue]): FbxValueArray = {
        val builder = scala.collection.mutable.ArrayBuilder.make[FbxValue]
        builder ++= elems
        def loop(): FbxValueArray = {
          val value = tokens.next() match {
            case Token.Long(v) => FbxLong(v)
            case Token.Double(v) => FbxDouble(v)
            case Token.String(v) => FbxString(v)
            case Token.Identifier(v) => FbxIdentifier(v)
            case t => throw new ParseError(s"Expected value but found '$t'")
          }
          builder += value
          if (tokens.nextIf(Token.Char(','))) {
            loop()
          } else FbxValueArray(builder.result())
        }
        loop()
      }

      tokens.peek() match {
        case Token.Char(_) => FbxValueArray(Array.empty)
        case Token.Identifier(v) if v.size > 1 => FbxValueArray(Array.empty)
        case Token.Long(_) => parseLongArray()
        case Token.Double(_) => parseDoubleArray()
        case _ => parseValueArray(Array.empty)
      }
    }

    FbxNode("", FbxValueArray(Array.empty), parseNodes())
  }
}