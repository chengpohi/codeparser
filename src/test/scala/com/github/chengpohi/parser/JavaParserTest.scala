package com.github.chengpohi.parser

import com.github.chengpohi.parser.java.JavaParser
import com.github.chengpohi.util.FileUtils._
import fastparse.core.Parsed
import org.scalatest.FlatSpec

/**
  * codeparser
  * Created by chengpohi on 5/5/16.
  */
class JavaParserTest extends FlatSpec {
  val javaParser = new JavaParser
  val testClassSource: String = readTestFile("/basic.java")
  val typeClassSource: String = readTestFile("/Type.java")
  val lambdaClassSource: String = readTestFile("/Lambda.java")
  val vargs: String = readTestFile("/Vargs.java")
  val test: String = readTestFile("/Test.java")

  "Java Parser" should "parse java source file" in {
    check(testClassSource)
  }

  "Java Parser" should "parse types" in {
    check(typeClassSource)
  }

  "Java Parser" should "parse lambda class" in {
    check(lambdaClassSource)
  }

  "Java Parser" should "parse vargs" in {
    check(vargs)
  }

  "Java Parser" should "parse test" in {
    check(test)
  }


  "Java Parser" should "parse java classes" in {
    val testDir = "/Users/xiachen/IdeaProjects/9dev/"
    walkFiles(testDir).filter(p => p.getFileName.toString.endsWith(".java")).foreach(f => {
      println(f.toString)
      check(readTestFile(f))
    })
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
