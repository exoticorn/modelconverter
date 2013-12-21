package de.exoticorn.modelconverter

import scala.io.Source
import de.exoticorn.modelconverter.fbx._

object Main extends App {
  val source = Source.fromFile("test.fbx")
  val lexer = new Lexer(source)
  val map = Parser.parse(lexer)
  println(map)
}
