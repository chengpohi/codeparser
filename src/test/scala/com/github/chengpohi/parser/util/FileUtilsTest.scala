package com.github.chengpohi.parser.util

import java.nio.file.{Path, Paths}

import com.github.chengpohi.util.FileUtils
import org.scalatest.FlatSpec

/**
  * codeparser
  * Created by chengpohi on 5/14/16.
  */
class FileUtilsTest extends FlatSpec {
  "walkFiles" should "list all files" in {
    val testPath = Paths.get(getClass.getResource("/").toURI)
    val filterPath = (p: Path) => p.getFileName.toString.endsWith(".java")
    val paths: Stream[Path] =
      FileUtils.walkFiles(testPath).filter(filterPath)
    assert(paths.nonEmpty)
  }
}
