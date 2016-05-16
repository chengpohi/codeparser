package com.github.chengpohi.util

import java.nio.file.{Files, Path, Paths}

import scala.io.Source

/**
  * codeparser
  * Created by chengpohi on 5/14/16.
  */
object FileUtils {
  def readTestFile(fileName: String): String = {
    Source.fromURL(getClass.getResource(fileName)).getLines().mkString("\n")
  }

  def readTestFile(path: Path): String = Source.fromFile(path.toFile).getLines().mkString("\n")

  def walkFiles(dir: String): Stream[Path] = walkFiles(Paths.get(dir))

  def walkFiles(dir: Path): Stream[Path] = {
    def listFile(its: java.util.Iterator[Path]): Stream[Path] = {
      its.hasNext match {
        case true =>
          val n = its.next()
          n #:: listFile(its)
        case false => Stream.empty
      }
    }
    val its: java.util.Iterator[Path] = Files.walk(dir).iterator()
    listFile(its)
  }

}
