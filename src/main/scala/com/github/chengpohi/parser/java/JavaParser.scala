package com.github.chengpohi.parser.java

import com.github.chengpohi.parser.java.syntax.{Core, Exprs, Types}
import fastparse.noApi._

import scala.language.implicitConversions

/**
  * Parser for Scala syntax.
  */
class JavaParser extends Core with Types with Exprs {

  import WhitespaceApi._

  val TmplBody: P0 = {
    val Prelude = P((Annot ~ OneNLMax).rep ~ (Mod ~/ Pass).rep)
    val TmplStat = P(Import | Prelude ~ BlockDef | StatCtx.Expr)

    P("{" ~/ BlockLambda.? ~ Semis.? ~ TmplStat.repX(sep = Semis) ~ Semis.? ~ `}`)
  }

  val ValVarDef = P(BindPattern.rep(1, ",".~/) ~ (`:` ~/ Type).? ~ (`=` ~/ StatCtx.Expr).?)

  val FunDef = {
    val Body = P(WL ~ `=` ~/ `macro`.? ~ StatCtx.Expr | OneNLMax ~ "{" ~ Block ~ "}")
    P(FunSig ~ (`:` ~/ Type).? ~~ Body.?)
  }

  val BlockDef: P0 = P(Dcl | InterfaceDef | ClsDef | ObjDef)

  val ClsDef = {
    val ClsAnnot = P(`@` ~ SimpleType ~ ArgList.?)
    val Prelude = P(NotNewline ~ (ClsAnnot.rep(1) ~ AccessMod.? | AccessMod))
    val ClsArgMod = P(Mod.rep ~ (`val` | `var`))
    val ClsArg = P(Annot.rep ~ ClsArgMod.? ~ Id ~ `:` ~ Type ~ (`=` ~ ExprCtx.Expr).?)

    val ClsArgs = P(OneNLMax ~ "(" ~/ `implicit`.? ~ ClsArg.rep(sep = ",".~/) ~ ")")
    P(`class` ~/ Id ~~ Prelude.? ~~ ClsArgs.repX ~ DefTmpl.?)
  }

  val Constrs = P((WL ~ Constr).rep(1, `with`.~/))
  val EarlyDefTmpl = P(TmplBody ~ (`with` ~/ Constr).rep ~ TmplBody.?)
  val NamedTmpl = P(Constrs ~ TmplBody.?)

  val DefTmpl = P((`extends` | `<:`) ~ AnonTmpl | TmplBody)
  val AnonTmpl = P(EarlyDefTmpl | NamedTmpl | TmplBody)

  val InterfaceDef = P(`interface` ~/ Id ~ TypeArgList.? ~ DefTmpl.?)

  val ObjDef: P0 = P(`case`.? ~ `object` ~/ Id ~ DefTmpl.?)

  val Constr = P(AnnotType ~~ (NotNewline ~ ParenArgList).repX)

  val PkgObj = P(ObjDef)
  val PkgBlock = P(QualId ~/ `{` ~ TopStatSeq.? ~ `}`)
  val Pkg = P(`package` ~/ (PkgBlock | PkgObj))
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
