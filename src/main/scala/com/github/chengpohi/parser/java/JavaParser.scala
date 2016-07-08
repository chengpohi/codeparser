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

  val TmplBlock: Parser[ClazzTree] = {
    //class state or annotation
    val Prelude = P((Annot ~ OneNLMax).rep ~ (Mod ~/ Pass).rep ~ BlockDef).map(i => i._3)
    val TmplStat = P(Prelude | StatCtx.Expr).map(i => i)


    P(BlockLambda.? ~ Semis.? ~ TmplStat.repX(sep = Semis) ~ Semis.?).map(i => Elements(i._2:_*))
  }


  //class/interface/abstract class Body
  val TmplBody: Parser[ClazzTree] = P("{" ~/ TmplBlock ~ `}`)

  val VarDefine: Parser[FieldDefine]= P((`=` ~/ StatCtx.Expr).?).map(i => FieldDefine(i.getOrElse(Element("FieldDefine"))))

  val FunDef: Parser[MethodDefine] = {
    val Body = P(WL ~ OneNLMax ~ "{" ~ Block ~ "}")
    P(FunSig ~~ Body.?).map(i => MethodDefine("MethodDefine"))
  }

  val BlockDef: Parser[ClazzTree] = P(InterfaceDef | EnumDef | ClsDef | Construct | Dcl | TmplBody)

  val ClsDef = {
    P(`class` ~/ Id.! ~ GenericArgList.? ~ DefTmpl.?).map(i => ClazzElements(ClazzName(i._1), i._3.getOrElse(null)))
  }

  val Constrs = P((WL ~ Constr).rep(1, `implements`.~/)).map(i => EMPTY_CLAZZ_TREES)
  val NamedTmpl: Parser[ClazzTree] = P(Constrs ~ TmplBody.?).map(i => i._2.getOrElse(null))
  val DefTmpl: Parser[ClazzTree] = P((`extends` | `implements`).? ~ AnonTmpl | TmplBody)
  val AnonTmpl: Parser[ClazzTree] = P(NamedTmpl | TmplBody)

  val InterfaceDef = P(`interface` ~/ Id.! ~ TypeArgList.? ~ DefTmpl.?)
    .map(i => ClazzElements(ClazzName(i._1), null))
  val EnumDef = P(`enum` ~/ Id.! ~ "{" ~/ BlockLambda.? ~ Semis.? ~ Id.repX(sep = ",") ~ ";".? ~ TmplBlock.? ~ `}`)
    .map(i => ClazzElements(ClazzName(i._1), null))

  val Constr = P(AnnotType)

  val PkgBlock = P(QualId ~/ `{` ~ TopStatSeq.? ~ `}`)
  val Pkg = P(`package` ~/ PkgBlock)
  val TopStatSeq: Parser[Any] = {
    //annotation and class statement
    //Annotation capture, class capture, mod
    val Tmpl = P((Annot ~~ OneNLMax).rep ~ Mod.rep ~ (InterfaceDef | ClsDef | EnumDef))
      .map(i => {
        val clazzElements: (ClazzName, ClazzTree) = i._3.asInstanceOf[ClazzElements].value
        Clazz(i._2, clazzElements._1, clazzElements._2)
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
