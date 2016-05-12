package com.github.chengpohi.parser

import scala.io.Source

/**
  * codeparser
  * Created by chengpohi on 5/12/16.
  */
object TestFileReader {
  def readTestFile(fileName: String): String = {
    Source.fromURL(getClass.getResource(fileName)).getLines().mkString("\n")
  }
}
