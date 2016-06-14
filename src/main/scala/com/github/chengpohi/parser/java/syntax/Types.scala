package com.github.chengpohi.parser.java.syntax

import com.github.chengpohi.parser.java.JavaAST.{AccessModifier, ClazzTree, Field}
import fastparse.noApi._

trait Types extends Core {

  import WhitespaceApi._

  def TypeExpr: Parser[Any]

  def VarDefine: Parser[Any]

  def FunDef: Parser[Any]

  def ArrayExpr: Parser[Any]

  //modifiers for class
  val LocalMod: Parser[Any] = P(`abstract` | `final` | `static`)
  val AccessMod: Parser[Any] = {
    val AccessQualifier = P("[" ~/ (`this` | Id) ~ "]")
    P((`public` | `private` | `protected`) ~ AccessQualifier.?)
  }
  val Dcl: Parser[ClazzTree] = P(Pass ~ Type.rep.! ~ Id.rep(sep = ",".~/) ~ (FunDef | VarDefine).?).map(i => Field(i._1))

  val Mod = P(LocalMod | AccessMod | `override`).!.map(AccessModifier)

  val ExistentialClause = P(`forSome` ~/ `{` ~ Dcl.repX(1, Semis) ~ `}`)
  val PostfixType = P(InfixType)
  val Type: Parser[Any] = P(PostfixType)


  // Can't cut after `Id` because it may be a `*`, in which case
  // we may need to backtrack and settle for the `*`-postfix rather than
  // an infix type
  val InfixType = P(NotNewline ~ SimpleType)

  val CompoundType = {
    val NamedType = P((Pass ~ AnnotType).rep(1, `with`.~/))
    P(NamedType)
  }
  val NLAnnot = P(NotNewline ~ Annot)
  val AnnotType = P(SimpleType ~~ NLAnnot.repX)

  val TypeId = P(StableId)

  val SimpleType: Parser[Any] = {
    val BasicType = P(TypeId ~ ArrayExpr.?)
    P(BasicType ~ TypeArgs.?)
  }

  val TypeArgs = P("<" ~ Type.rep(sep = ",".~/) ~ ">")


  val Exception = P(`throws` ~ Type.rep(1, sep = ","))
  val FunSig: Parser[Any] = {
    val FunArg = P(Annot.rep ~ `final`.? ~ Type ~ WL ~ Id)
    val Args = P(FunArg.rep(1, ",".~/))
    val FunTypeArgs = P("[" ~/ (Annot.rep ~ TypeArg).rep(1, ",".~/) ~ "]")
    P(OneNLMax ~ "(" ~/ Args.? ~ ")" ~/ Exception.?)
  }

  val TypeBounds: Parser[Any] = P((Pass ~ `extends` ~/ Type).?)
  val TypeArg: Parser[Any] = {
    val CtxBounds = P((`<%` ~/ Type).rep ~ (`:` ~/ Type).rep)
    P((Id | `_`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  //class annotation with name, values
  val Annot: Parser[Any] = P(`@` ~/ SimpleType ~ ("(" ~/ Exprs.? ~ ")").rep)

  val TypeArgList: Parser[Any] = {
    val Variant: Parser[Any] = P(Annot.rep ~ CharIn("+-").? ~ TypeArg)
    P("[" ~/ Variant.rep(1, ",".~/) ~ "]")
  }

  val GenericArgList: Parser[Any] = {
    val Variant: Parser[Any] = P(Annot.rep ~ CharIn("+-").? ~ TypeArg)
    P("<" ~/ Variant.rep(1, ",".~/) ~ ">")
  }
  val Exprs: Parser[Any] = P(TypeExpr.rep(1, ",".~/))
  val TypeDef: Parser[Any] = P(Id ~ TypeArgList.? ~ (`=` ~/ Type | TypeBounds))
}
