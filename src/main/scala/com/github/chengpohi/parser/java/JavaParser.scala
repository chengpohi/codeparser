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

  val TmplBlock: Parser[Seq[ClazzTree]] = {
    //class state or annotation
    val Prelude = P((Annot ~ OneNLMax).rep ~ (Mod ~/ Pass).rep ~ BlockDef).map(i => i._3)
    //val TmplStat = P(Prelude ~ BlockDef).map(i => i._3)
    val TmplStat = P(Prelude | StatCtx.Expr).map(i => i)


    P(BlockLambda.? ~ Semis.? ~ TmplStat.repX(sep = Semis) ~ Semis.?).map(i => i._2)
    //P(BlockLambda.? ~ Semis.? ~ TmplStat.repX(sep = Semis) ~ Semis.?).map(i => Seq(Field(("TestField", "ff"))))
  }


  //class/interface/abstract class Body
  val TmplBody: Parser[Seq[ClazzTree]] = P("{" ~/ TmplBlock ~ `}`)

  val VarDefine: Parser[FieldDefine]= P((`=` ~/ StatCtx.Expr).?).map(i => FieldDefine(i.getOrElse(Element("FieldDefine"))))

  val FunDef: Parser[MethodDefine] = {
    val Body = P(WL ~ OneNLMax ~ "{" ~ Block ~ "}")
    P(FunSig ~~ Body.?).map(i => MethodDefine("MethodDefine"))
  }

  val BlockDef: Parser[ClazzTree] = P(InterfaceDef | EnumDef | ClsDef | Dcl)

  val ClsDef = {
    P(`class` ~/ Id.! ~ GenericArgList.? ~ DefTmpl.?).map(i => ClazzElements(ClazzName(i._1), i._3.getOrElse(Seq())))
  }

  val Constrs = P((WL ~ Constr).rep(1, `implements`.~/))
  val NamedTmpl = P(Constrs ~ TmplBody.?)
  //TODO super class and super interface
  val DefTmpl: Parser[Seq[ClazzTree]] = P((`extends` | `implements`).? ~ TmplBody)
  val AnonTmpl = P(NamedTmpl | TmplBody)

  val InterfaceDef = P(`interface` ~/ Id.! ~ TypeArgList.? ~ DefTmpl.?).map(i => ClazzName(i._1))
  val EnumDef = P(`enum` ~/ Id.! ~ "{" ~/ BlockLambda.? ~ Semis.? ~ Id.repX(sep = ",") ~ ";".? ~ TmplBlock.? ~ `}`).map(i => ClazzName(i._1))

  val Constr = P(AnnotType)

  val PkgBlock = P(QualId ~/ `{` ~ TopStatSeq.? ~ `}`)
  val Pkg = P(`package` ~/ PkgBlock)
  val TopStatSeq: Parser[Any] = {
    //annotation and class statement
    //Annotation capture, class capture, mod
    val Tmpl = P((Annot ~~ OneNLMax).rep ~ Mod.? ~ (InterfaceDef | ClsDef | EnumDef))
      .map(i => {
        val clazzElements: (ClazzName, Seq[ClazzTree]) = i._3.asInstanceOf[ClazzElements].value
        Clazz(i._2.getOrElse(AccessModifier("public")), clazzElements._1, clazzElements._2)
      })
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
