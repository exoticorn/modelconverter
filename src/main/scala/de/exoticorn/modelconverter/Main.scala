package de.exoticorn.modelconverter

import scala.io.Source
import de.exoticorn.modelconverter.fbx.Lexer
import de.exoticorn.modelconverter.fbx.Token

object Main extends App {
  val source = Source.fromFile("test.fbx")
  val lexer = new Lexer(source)
  def loop() {
    val t = lexer.nextToken()
    println(t)
    if (t != Token.EOF) {
      loop()
    }
  }
  loop()
}
