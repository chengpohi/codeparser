package com.github.chengpohi.parser.java

import com.github.chengpohi.parser.java.JavaAST._
import com.github.chengpohi.parser.java.syntax.{Core, Exprs, Types}
import fastparse.noApi._

import scala.language.implicitConversions

/**
  * Parser for Scala syntax.
  */
class JavaParser extends Core with Types with Exprs {

  import WhitespaceApi._

  val TmplBlock: Parser[Any] = {
    //class state or annotation
    val Prelude = P((Annot ~ OneNLMax).rep ~ (Mod ~/ Pass).rep)
    val TmplStat = P(Prelude ~ BlockDef | StatCtx.Expr)

    P(BlockLambda.? ~ Semis.? ~ TmplStat.repX(sep = Semis) ~ Semis.?)
  }


  //class/interface/abstract class Body
  val TmplBody: Parser[Any] = P("{" ~/ TmplBlock ~ `}`)

  val VarDefine = P((`=` ~/ StatCtx.Expr).?)

  val FunDef = {
    val Body = P(WL ~ OneNLMax ~ "{" ~ Block ~ "}")
    P(FunSig ~~ Body.?)
  }

  val BlockDef: Parser[Any] = P(InterfaceDef | EnumDef | ClsDef | Dcl)

  val ClsDef = {
    P(`class` ~/ Id.! ~ GenericArgList.? ~ DefTmpl.?).map(i => ClazzName(i._1))
  }

  val Constrs = P((WL ~ Constr).rep(1, `implements`.~/))
  val NamedTmpl = P(Constrs ~ TmplBody.?)
  val DefTmpl = P((`extends` | `implements`) ~ AnonTmpl | TmplBody)
  val AnonTmpl = P(NamedTmpl | TmplBody)

  val InterfaceDef = P(`interface` ~/ Id.! ~ TypeArgList.? ~ DefTmpl.?).map(i => ClazzName(i._1))
  val EnumDef = P(`enum` ~/ Id.! ~ "{" ~/ BlockLambda.? ~ Semis.? ~ Id.repX(sep = ",") ~ ";".? ~ TmplBlock.? ~ `}`).map(i => ClazzName(i._1))

  val Constr = P(AnnotType)

  val PkgBlock = P(QualId ~/ `{` ~ TopStatSeq.? ~ `}`)
  val Pkg = P(`package` ~/ PkgBlock)
  val TopStatSeq: Parser[Any] = {
    //annotation and class statement
    //Annotation capture, class capture, mod
    val Tmpl = P((Annot ~~ OneNLMax).rep ~ Mod.? ~ (InterfaceDef | ClsDef | EnumDef)).map(i => Clazz(i._2, i._3))
    val TopStat = P(Pkg | Import | Tmpl)
    P(TopStat.repX(1, Semis))
  }
  //~~ parse with space newline tab
  //parse package with comment
  val TopPkgSeq = P(`package` ~ QualId)
  val CompilationUnit: Parser[Any] = {
    val Body = P(TopPkgSeq ~~ (Semis ~ TopStatSeq).? | TopStatSeq)
    P(Semis.? ~ Body.? ~ WL ~ End)
  }
}
