package com.github.chengpohi.parser.java

/**
  * codeparser
  * Created by chengpohi on 6/12/16.
  */
object JavaAST {

  sealed trait ClazzTree extends Any {
    def value: Any
  }

  case class Clazz(modifier: Option[Modifier] = None, clazzName: ClazzName)

  case class ClazzName(value: java.lang.String) extends AnyVal with ClazzTree

  case class Modifier(value: java.lang.String) extends AnyVal with ClazzTree

  case class Import(value: java.lang.String) extends AnyVal with ClazzTree

  case class Method(value: java.lang.String) extends AnyVal with ClazzTree

}
