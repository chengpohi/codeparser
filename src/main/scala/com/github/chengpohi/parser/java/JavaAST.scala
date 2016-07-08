package com.github.chengpohi.parser.java

/**
  * codeparser
  * Created by chengpohi on 6/12/16.
  */
object JavaAST {

  sealed trait ClazzTree extends Any {
    def value: Any
  }

  case class Clazz(accessModifier: Seq[AccessModifier], clazzName: ClazzName, elements: ClazzTree)

  case class ClazzName(value: String) extends AnyVal with ClazzTree

  case class AccessModifier(value: String) extends AnyVal with ClazzTree

  case class ClazzElements(value: (ClazzName, ClazzTree)) extends AnyVal with ClazzTree

  case class Field(value: (String, String)) extends AnyVal with ClazzTree

  case class FieldDefine(value: ClazzTree) extends AnyVal with ClazzTree

  case class Element(value: String) extends AnyVal with ClazzTree

  case class Elements(value: ClazzTree*) extends AnyVal with ClazzTree

  case class Import(value: String) extends AnyVal with ClazzTree

  case class Method(value: (String, String)) extends AnyVal with ClazzTree

  case class Constructor(value: String) extends AnyVal with ClazzTree

  case class MethodDefine(value: String) extends AnyVal with ClazzTree

  case class StaticDefine(value: String) extends AnyVal with ClazzTree

  val EMPTY_CLAZZ_TREES = Seq[ClazzTree]()
  val EMPTY_ELEMENT = Element("EMPTY_ELEMENT")
}
