package com.github.chengpohi.parser

import com.github.chengpohi.parser.java.JavaAST.{AccessModifier, Clazz, ClazzName, ClazzTree, Constructor, Elements, Field, Method}
import com.github.chengpohi.parser.java.JavaParser
import com.github.chengpohi.util.FileUtils._
import fastparse.core.Parsed
import fastparse.core.Parsed.Success
import org.scalatest.FlatSpec

import scala.collection.mutable.ArrayBuffer

/**
  * codeparser
  * Created by chengpohi on 5/5/16.
  */
class JavaParserTest extends FlatSpec {
  val javaParser = new JavaParser

  "Java Parser" should "parse java source file" in {
    val testClassSource: String = readTestFile("/basic.java")
    check(testClassSource)
  }

  "Java Parser" should "parse types" in {
    val typeClassSource: String = readTestFile("/Type.java")
    check(typeClassSource)
  }

  "Java Parser" should "parse lambda class" in {
    val lambdaClassSource: String = readTestFile("/Lambda.java")
    check(lambdaClassSource)
  }

  "Java Parser" should "parse vargs" in {
    val vargs: String = readTestFile("/Vargs.java")
    check(vargs)
  }

  "Java Parser" should "parse enum" in {
    val enum: String = readTestFile("/Enum.java")
    check(enum)
  }

  "Java Parser" should "parse switch" in {
    val switch: String = readTestFile("/Switch.java")
    check(switch)
  }

  "Java Parser" should "parse test" in {
    val test: String = readTestFile("/Test.java")
    check(test)
  }


  "Java Parser" should "parse java classes" in {
    val testDir = "/Users/xiachen/IdeaProjects/9dev/"
    walkFiles(testDir).filter(p => p.getFileName.toString.endsWith(".java")).zipWithIndex.foreach(f => {
      println(f._2 + " " +  f._1.toString)
      check(readTestFile(f._1))
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

  def checkAST(input: String, target: Any) = {
    val res = javaParser.CompilationUnit.parse(input)
    res match {
      case f: Parsed.Failure =>
        throw new Exception(input + "\n" + f.extra.traced.trace)
      case Success(tree, f) =>
        println(tree)
        assert(tree === target)
    }
  }
}
