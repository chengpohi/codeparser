package com.github.chengpohi.parser.java

import com.github.chengpohi.parser.java.syntax.{Core, Exprs, Types}
import fastparse.noApi._

import scala.language.implicitConversions

/**
  * Parser for Scala syntax.
  */
class JavaParser extends Core with Types with Exprs {

  import WhitespaceApi._

  //class/interface/abstract class Body
  val TmplBody: P0 = {
    //class state or annotation
    val Prelude = P((Annot ~ OneNLMax).rep ~ (Mod ~/ Pass).rep)
    val TmplStat = P(Prelude ~ BlockDef | StatCtx.Expr)

    P("{" ~/ BlockLambda.? ~ Semis.? ~ TmplStat.repX(sep = Semis) ~ Semis.? ~ `}`)
  }

  val VarDefine = P((`=` ~/ StatCtx.Expr).?)

  val FunDef = {
    val Body = P(WL ~ OneNLMax ~ "{" ~ Block ~ "}")
    P(FunSig ~~ Body.?)
  }

  val BlockDef: P0 = P(Dcl | InterfaceDef | ClsDef)

  val ClsDef = {
    val ClsAnnot = P(`@` ~ SimpleType ~ ArgList.?)
    val Prelude = P(NotNewline ~ (ClsAnnot.rep(1) ~ AccessMod.? | AccessMod))

    P(`class` ~/ Id ~~ Prelude.? ~ DefTmpl.?)
  }

  val Constrs = P((WL ~ Constr).rep(1, `implements`.~/))
  val NamedTmpl = P(Constrs ~ TmplBody.?)
  val DefTmpl = P((`extends` | `implements`) ~ AnonTmpl | TmplBody)
  val AnonTmpl = P(NamedTmpl | TmplBody)

  val InterfaceDef = P(`interface` ~/ Id ~ TypeArgList.? ~ DefTmpl.?)

  val Constr = P(AnnotType)

  val PkgBlock = P(QualId ~/ `{` ~ TopStatSeq.? ~ `}`)
  val Pkg = P(`package` ~/ PkgBlock)
  val TopStatSeq: P0 = {
    //annotation and class statement
    val Tmpl = P((Annot ~~ OneNLMax).rep ~ Mod.rep ~ (InterfaceDef | ClsDef))
    val TopStat = P(Pkg | Import | Tmpl)
    P(TopStat.repX(1, Semis))
  }
  //~~ parse with space newline tab
  //parse package with comment
  val TopPkgSeq = P(`package` ~ QualId)
  val CompilationUnit: P0 = {
    val Body = P(TopPkgSeq ~~ (Semis ~ TopStatSeq).? | TopStatSeq)
    P(Semis.? ~ Body.? ~ WL ~ End)
  }
}
