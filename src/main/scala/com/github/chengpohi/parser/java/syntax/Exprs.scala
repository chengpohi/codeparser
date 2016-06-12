package com.github.chengpohi.parser.java.syntax

import fastparse.noApi._

trait Exprs extends Core with Types {

  import WhitespaceApi._

  def AnonTmpl: Parser[Any]

  def BlockDef: Parser[Any]

  def TmplBody: Parser[Any]

  val Import: Parser[Any] = {
    val Selector: Parser[Any] = P((Id | `_`) ~ (`=>` ~/ (Id | `_`)).?)
    val Selectors: Parser[Any] = P("{" ~/ Selector.rep(sep = ",".~/) ~ "}")
    val ImportExpr: Parser[Any] = P(StableId ~ ("." ~/ (`_` | Selectors)).?)
    P(`import` ~/ ImportExpr.rep(1, sep = ",".~/))
  }

  val arrayInitBlock: Parser[Any] = P("{" ~ ExprCtx.Expr.rep(sep = ",") ~ "}")
  val ArrayExpr: Parser[Any] = P(("[" ~/ ExprCtx.Expr.? ~ "]").rep(1) ~ arrayInitBlock.?)

  object StatCtx extends WsCtx(curlyBlock = true)

  object ExprCtx extends WsCtx(curlyBlock = false)

  val TypeExpr = ExprCtx.Expr

  class WsCtx(curlyBlock: Boolean) {

    val OneSemiMax = if (curlyBlock) OneNLMax else Pass
    val NoSemis = if (curlyBlock) NotNewline else Pass


    val Enumerators = {
      val Generator = P(`<-` ~/ Expr ~ Guard.?)
      val Assign = P(`=` ~/ Expr)
      val Enumerator = P(Semis ~ `val`.? ~ TypeOrBindPattern ~/ (Generator | Assign) | Semis.? ~ Guard)
      P(TypeOrBindPattern ~ Generator ~~ Enumerator.repX)
    }

    val Expr: Parser[Any] = {
      val If = {
        val Else = P(Semi.? ~ `else` ~/ Expr)
        P(`if` ~/ "(" ~ ExprCtx.Expr ~ ")" ~ Expr ~ Else.?)
      }

      val Break = P("break")
      val CaseClause: Parser[Any] = P(`case` ~/ ExprCtx.Expr ~ `:` ~/ Block)
      val DefaultClause: Parser[Any] = P(`default` ~/ `:` ~/ Block)

      val While = P(`while` ~/ "(" ~ ExprCtx.Expr ~ ")" ~ Expr)
      val Switch = {
        P(`switch` ~/ "(" ~ ExprCtx.Expr ~ ")" ~ "{" ~/ Expr ~ "}")
      }
      val Try = {
        val Catch = P(`catch` ~/ "(" ~ Type ~ WL ~ Id ~ ")" ~ "{" ~/ BlockChunk ~ "}")
        val Finally = P(`finally` ~/ Expr)
        P(`try` ~/ Expr ~ Catch.rep ~ Finally.?)
      }
      val DoWhile = P(`do` ~/ Expr ~ Semi.? ~ `while` ~ "(" ~ ExprCtx.Expr ~ ")")

      val For = {
        val Body1 = P(ExprCtx.Expr.? ~ ";" ~ ExprCtx.Expr.? ~ ";" ~ ExprCtx.Expr.?)
        val Body2 = P(ExprCtx.Expr ~ `:` ~ ExprCtx.Expr)
        val Loop = P(("(" ~ (Body1 | Body2) ~ ")") ~ OneNLMax ~ Expr)
        P(`for` ~/ Loop)
      }
      val Throw = P(`throw` ~/ Expr)
      val Return = P(`return` ~/ Expr.?)
      val LambdaRhs = if (curlyBlock) P(BlockChunk) else P(Expr)

      val TernaryExpr = {
        P("?" ~/ ExprCtx.Expr ~ `:` ~/ ExprCtx.Expr)
      }

      val ImplicitLambda = P(`implicit` ~ (Id | `_`) ~ (`:` ~ InfixType).? ~ `=>` ~ LambdaRhs.?)
      val ParenedLambda = P(Parened ~~ (WL ~ `=>` ~ LambdaRhs.? | ExprSuffix ~~ PostfixSuffix))
      val PostfixLambda = P(PostfixExpr ~ (`=>` ~ LambdaRhs.?).?)
      val MethodCall = P(Type ~ "(" ~ ExprCtx.Expr.rep(sep = ",") ~ ")")
      val SmallerExprOrLambda = P(ParenedLambda | PostfixLambda | MethodCall)
      val CastClause = P("(" ~ Type ~ ")" ~ ExprCtx.Expr)
      P(
        If | CaseClause | DefaultClause | Switch | Break | While | Try | DoWhile | For | Throw | Return | CastClause |
          ImplicitLambda | SmallerExprOrLambda ~ TernaryExpr.?
      )
    }
    val AscriptionType = if (curlyBlock) P(PostfixType) else P(Type)
    val Ascription = P(`:` ~/ (`_*` | AscriptionType | Annot.rep(1)))
    val ExprPrefix = P(WL ~ CharIn("-+!~") ~~ !Basic.OpChar ~ WS)
    val ArraySuffix = P("[" ~/ ExprCtx.Expr ~ "]" ~ ("." ~ Expr).?)
    val ExprSuffix = P((WL ~ "." ~/ Id | WL ~ ArraySuffix | NoSemis ~ ArgList).repX ~~ (NoSemis ~ `_`).?)
    val PrefixExpr = P(ExprPrefix.? ~ SimpleExpr)

    // Intermediate `WL` needs to always be non-cutting, because you need to
    // backtrack out of `InfixSuffix` into `PostFixSuffix` if it doesn't work out
    val InfixSuffix = P(NoSemis ~~ WL ~~ Id ~ TypeArgs.? ~~ OneSemiMax ~ PrefixExpr ~~ ExprSuffix)
    val PostFix = P(NoSemis ~~ WL ~~ Id ~ Newline.?)

    val PostfixSuffix = P(InfixSuffix.repX ~~ PostFix.? ~ (`=` ~/ Expr).?)

    val PostfixExpr: Parser[Any] = P(PrefixExpr ~~ ExprSuffix ~~ PostfixSuffix)

    val Parened = P("(" ~/ TypeExpr.rep(0, ",".~/) ~ ")")
    val InitialBlock: Parser[Any] = P("{" ~ StatCtx.Expr.repX(sep = ",") ~ "}")

    val SimpleExpr: Parser[Any] = {

      val New = P(`new` ~/ AnnotType ~ InitialBlock.?)

      P(New | BlockExpr | ExprLiteral | StableId | `_` | Parened)
    }
    val Guard: Parser[Any] = P(`if` ~/ PostfixExpr)
  }

  val SimplePattern: Parser[Any] = {
    val TupleEx = P("(" ~/ Pattern.rep(sep = ",".~/) ~ ")")
    val Extractor = P(StableId ~ TypeArgs.? ~ TupleEx.?)
    val Thingy = P(`_` ~ (`:` ~/ TypePat).? ~ !("*" ~~ !Basic.OpChar))
    P(Thingy | PatLiteral | TupleEx | Extractor | VarId)
  }

  val BlockExpr: Parser[Any] = P("{" ~/ Block ~ "}")

  val BlockLambdaHead: Parser[Any] = P("(" ~ BlockLambdaHead ~ ")" | `this` | Id | `_`)
  val BlockLambda = P(BlockLambdaHead ~ (`=>` | `:` ~ InfixType ~ `=>`.?))

  val BlockChunk = {
    val Prelude = P(Annot.rep ~ `implicit`.? ~ `lazy`.? ~ LocalMod.rep)
    val BlockStat = P(Import | Prelude ~ StatCtx.Expr | BlockDef)
    P(BlockLambda.rep ~ BlockStat.rep(sep = Semis))
  }

  val Block: Parser[Any] = {
    val BlockEnd = P(Semis.? ~ &("}" | `case`))
    val Body = P(BlockChunk.repX(sep = Semis))
    P(Semis.? ~ BlockLambda.? ~ Body ~/ BlockEnd)
  }

  val Patterns: Parser[Any] = P(Pattern.rep(1, sep = ",".~/))
  val Pattern: Parser[Any] = P((WL ~ TypeOrBindPattern).rep(1, sep = "|".~/))
  val TypePattern = P((`_` | VarId) ~ `:` ~ TypePat)
  val TypeOrBindPattern: Parser[Any] = P(TypePattern | BindPattern)
  val BindPattern: Parser[Any] = {
    val InfixPattern = P(SimplePattern ~ (Id ~/ SimplePattern).rep | `_*`)
    val Binding = P((VarId | `_`) ~ `@`)
    P(Binding ~ InfixPattern | InfixPattern | VarId)
  }

  val TypePat = P(CompoundType)
  val ParenArgList = P("(" ~/ (Exprs ~ (`:` ~/ `_*`).?).? ~ ")")
  val ArgList: Parser[Any] = P(ParenArgList | OneNLMax ~ BlockExpr)

  val CaseClauses: Parser[Any] = {
    val CaseClause: Parser[Any] = P(`case` ~/ Id)
    P(CaseClause.rep(1) ~ "}")
  }
}
