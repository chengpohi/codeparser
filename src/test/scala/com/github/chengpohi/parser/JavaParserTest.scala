package com.github.chengpohi.parser

import org.scalatest.FlatSpec

import scala.io.Source

/**
  * codeparser
  * Created by chengpohi on 5/5/16.
  */
class JavaParserTest extends FlatSpec{
  private val testClassSource: String = Source.fromURL(getClass.getResource("/Test.java")).getLines().mkString("\\n")
  val javaParser  = new JavaParser

  "Java Parser" should "parse java source file" in {
    val parseResult: String = javaParser.parse(testClassSource)
  }

}
