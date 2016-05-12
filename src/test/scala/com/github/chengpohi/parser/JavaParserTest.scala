package com.github.chengpohi.parser

import com.github.chengpohi.parser.TestFileReader.readTestFile
import com.github.chengpohi.parser.java.JavaParser
import fastparse.core.Parsed
import org.scalatest.FlatSpec

/**
  * codeparser
  * Created by chengpohi on 5/5/16.
  */
class JavaParserTest extends FlatSpec {
  val javaParser = new JavaParser
  val testClassSource: String = readTestFile("/basic.java")
  private val string: String = readTestFile("/Type.java")


  val lambdaClassSource: String = string

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
