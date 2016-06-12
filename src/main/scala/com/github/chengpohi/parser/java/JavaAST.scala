package com.github.chengpohi.parser.java

/**
  * codeparser
  * Created by chengpohi on 6/12/16.
  */
object JavaAST {

  sealed trait Clazz extends Any {
    def value: Any
  }

  case class ClazzName(value: java.lang.String) extends AnyVal with Clazz

  case class Import(value: java.lang.String) extends AnyVal with Clazz

  case class Method(value: java.lang.String) extends AnyVal with Clazz

}
