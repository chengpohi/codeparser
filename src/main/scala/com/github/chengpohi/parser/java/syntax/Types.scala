package com.github.chengpohi.parser.java.syntax

import fastparse.noApi._

trait Types extends Core {

  import WhitespaceApi._

  def TypeExpr: P0

  def VarDefine: P0

  def FunDef: P0

  //modifiers for class
  val LocalMod: P0 = P(`abstract` | `final`)
  val AccessMod: P0 = {
    val AccessQualifier = P("[" ~/ (`this` | Id) ~ "]")
    P((`public` | `private` | `protected`) ~ AccessQualifier.?)
  }
  val Dcl: P0 = P(Pass ~ Type ~ Id.rep(0, ",".~/) ~ (FunDef | VarDefine).?)

  val Mod: P0 = P(LocalMod | AccessMod | `override`)

  val ExistentialClause = P(`forSome` ~/ `{` ~ Dcl.repX(1, Semis) ~ `}`)
  val PostfixType = P(InfixType)
  val Type: P0 = P(PostfixType)


  // Can't cut after `Id` because it may be a `*`, in which case
  // we may need to backtrack and settle for the `*`-postfix rather than
  // an infix type
  val InfixType = P(CompoundType ~~ (NotNewline ~ Id ~~ OneNLMax ~ SimpleType).repX)

  val CompoundType = {
    val NamedType = P((Pass ~ AnnotType).rep(1, `with`.~/))
    P(NamedType)
  }
  val NLAnnot = P(NotNewline ~ Annot)
  val AnnotType = P(SimpleType ~~ NLAnnot.repX)

  val TypeId = P(StableId)
  val SimpleType: P0 = {
    val BasicType = P(TypeId)
    P(BasicType ~ (Pass ~ (TypeArgs ~/ Id)).rep)
  }

  val TypeArgs = P("[" ~/ Type.rep(sep = ",".~/) ~ "]")


  val FunSig: P0 = {
    val FunArg = P(Annot.rep ~ Type ~ Id)
    val Args = P(FunArg.rep(1, ",".~/))
    P(OneNLMax ~ "(" ~/ Args.? ~ ")")
  }

  val TypeBounds: P0 = P((Pass ~ `extends` ~/ Type).? )
  val TypeArg: P0 = {
    val CtxBounds = P((`<%` ~/ Type).rep ~ (`:` ~/ Type).rep)
    P((Id | `_`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  //class annotation with name, values
  val Annot: P0 = P(`@` ~/ SimpleType ~ ("(" ~/ Exprs.? ~ ")").rep)

  val TypeArgList: P0 = {
    val Variant: P0 = P(Annot.rep ~ CharIn("+-").? ~ TypeArg)
    P("[" ~/ Variant.rep(1, ",".~/) ~ "]")
  }

  val GenericArgList: P0 = {
    val Variant: P0 = P(Annot.rep ~ CharIn("+-").? ~ TypeArg)
    P("<" ~/ Variant.rep(1, ",".~/) ~ ">")
  }
  val Exprs: P0 = P(TypeExpr.rep(1, ",".~/))
  val TypeDef: P0 = P(Id ~ TypeArgList.? ~ (`=` ~/ Type | TypeBounds))
}
