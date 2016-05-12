package com.github.chengpohi.parser

import com.github.chengpohi.parser.java.JavaParser
import fastparse.core.Parsed
import org.scalatest.FlatSpec

import scala.io.Source

/**
  * codeparser
  * Created by chengpohi on 5/5/16.
  */
class JavaParserTest extends FlatSpec {
  val javaParser = new JavaParser
  val testClassSource: String = Source.fromURL(getClass.getResource("/basic.java")).getLines().mkString("\n")
  val lambdaClassSource: String = Source.fromURL(getClass.getResource("/Lambda.java")).getLines().mkString("\n")

  "Java Parser" should "parse java source file" in {
    check(testClassSource)
  }
  "Java Parser" should "parse lambda class" in {
    check(lambdaClassSource)
  }

  def check(input: String) = {
    val res = javaParser.CompilationUnit.parse(input)
    res match {
      case f: Parsed.Failure =>
        throw new Exception(input + "\n" + f.extra.traced.trace)
      case s: Parsed.Success[_] =>
        val inputLength = input.length
        assert(s.index == inputLength)
    }
  }
}
