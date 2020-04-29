/*
 * This file is part of Dynamic David.
 *
 * (C) 2020 Mattias Ulbrich, Karlsruhe Institute of Technology
 *
 * This is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * DIVE is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with DIVE.  If not, see <http://www.gnu.org/licenses/>.
 *
 * @license GPL-3.0-or-later
 */
package edu.kit.iti.hilbert.parser

import java.io.InputStream

import edu.kit.iti.hilbert._
import org.antlr.v4.runtime.{CharStream, CharStreams, CommonTokenStream, RecognitionException}

import scala.collection.JavaConverters._

object HilbertParsers {

  def parseFile(file: String): Seq[Command] =
    parseProgram(CharStreams.fromFileName(file))

  def parseFile(is: InputStream): Seq[Command] =
    parseProgram(CharStreams.fromStream(is))

  def parseCommands(str: String): Seq[Command] =
    parseProgram(CharStreams.fromString(str))

  def parseFormula(str: String): Formula = {
    val input = CharStreams.fromString(str)
    val lexer = new HilbertLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new HilbertParser(tokens)
    val errorStrategy = new BailOutErrorStrategy
    parser.setErrorHandler(errorStrategy)
    try {
      val tree = parser.formula()
      tree.accept(ScalaVisitor).asInstanceOf[Formula]
    } catch {
      case ex: RecognitionException => errorStrategy.reportError(parser, ex)
        throw ex

      case rex: RuntimeException => if (rex.getCause.isInstanceOf[RecognitionException])
        errorStrategy.reportError(parser, rex.getCause.asInstanceOf[RecognitionException])
        throw rex
    }
  }

  def parseCommand(str: String): Command = {
    val input = CharStreams.fromString(str)
    val lexer = new HilbertLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new HilbertParser(tokens)
    val errorStrategy = new BailOutErrorStrategy
    parser.setErrorHandler(errorStrategy)
    try {
      val tree = parser.oneCommand()
      ScalaVisitor(tree.command())
    } catch {
      case ex: RecognitionException => errorStrategy.reportError(parser, ex)
        throw ex

      case rex: RuntimeException => if (rex.getCause.isInstanceOf[RecognitionException])
        errorStrategy.reportError(parser, rex.getCause.asInstanceOf[RecognitionException])
        throw rex
    }
  }

  private def parseProgram(input: CharStream): Seq[Command] = {
    val lexer = new HilbertLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new HilbertParser(tokens)
    val errorStrategy = new BailOutErrorStrategy
    parser.setErrorHandler(errorStrategy)
    try {
      val tree = parser.commandList()
      ScalaVisitor(tree)
    } catch {
      case ex: RecognitionException => errorStrategy.reportError(parser, ex)
        throw ex

      case rex: RuntimeException => if (rex.getCause.isInstanceOf[RecognitionException])
        errorStrategy.reportError(parser, rex.getCause.asInstanceOf[RecognitionException])
        throw rex
    }
  }
}

object ScalaVisitor extends HilbertBaseVisitor[AST] {

  def apply(ctx: HilbertParser.CommandListContext) : Seq[Command] =
    for(c <- asScalaBuffer(ctx.command()))
      yield c.accept(this).asInstanceOf[Command]

  def apply(ctx: HilbertParser.CommandContext) : Command =
    ctx.accept(this).asInstanceOf[Command]

  override def visitAssume(ctx: HilbertParser.AssumeContext): ASSUME =
    ASSUME(Option(ctx.ID()).map(_.getText),
      if(ctx.formula != null)
        Fact(ctx.formula.accept(this).asInstanceOf[Formula])
      else
        ctx.fact.accept(this).asInstanceOf[Fact])

  override def visitInstantiate(ctx: HilbertParser.InstantiateContext): INST = {
    val vars = for(v <- asScalaBuffer(ctx.vars)) yield v.getText
    val exps = for(e <- asScalaBuffer(ctx.exps)) yield e.accept(this).asInstanceOf[Formula]
    val pvars = for(v <- asScalaBuffer(ctx.progvars)) yield v.getText
    val progs = for(p <- asScalaBuffer(ctx.progs)) yield p.accept(this).asInstanceOf[Program]
    INST(Option(ctx.name).map(_.getText), ctx.id.getText,
      (vars zip exps).toMap, (pvars zip progs).toMap,
      Option(ctx.obtain).map(_.accept(this).asInstanceOf[Fact]))
  }

  override def visitCommand(ctx: HilbertParser.CommandContext): Command =
    ctx.children.get(0).accept(this).asInstanceOf[Command]

  override def visitShowFact(ctx: HilbertParser.ShowFactContext): PRINT_FACT =
    PRINT_FACT(Some(ctx.id.getText), ctx.proof != null)

  override def visitShowFacts(ctx: HilbertParser.ShowFactsContext): PRINT_FACT =
    PRINT_FACT(None, false)

  override def visitQuit(ctx: HilbertParser.QuitContext) = QUIT()

  override def visitHelp(ctx: HilbertParser.HelpContext) =
    HELP(Option(ctx.name).map(_.getText))

  override def visitClear(ctx: HilbertParser.ClearContext) =
    CLEAR(Option(ctx.name).map(_.getText))

  override def visitSet(ctx: HilbertParser.SetContext) =
    SET(ctx.name.getText,
      if(ctx.value.getType == HilbertParser.STRING_LIT)
        Interpreter.stripQuotes(ctx.value.getText) else ctx.value.getText)

  override def visitLoad(ctx: HilbertParser.LoadContext): LOAD =
    LOAD(ctx.STRING_LIT().getText, Option(ctx.ID).map(_.getText))

  override def visitScriptdef(ctx: HilbertParser.ScriptdefContext): SCRIPTDEF =
    SCRIPTDEF(ctx.SCRIPTID.getText, ctx.CODEBLOCK.getText)


  override def visitScriptcall(ctx: HilbertParser.ScriptcallContext): SCRIPT = {
    val vars = for(v <- asScalaBuffer(ctx.vars)) yield v.getText
    val exps = for(e <- asScalaBuffer(ctx.exps)) yield e.accept(this).asInstanceOf[Formula]
    val pvars = for(v <- asScalaBuffer(ctx.progvars)) yield v.getText
    val progs = for(p <- asScalaBuffer(ctx.progs)) yield p.accept(this).asInstanceOf[Program]
    SCRIPT(Option(ctx.ID).map(_.getText), ctx.SCRIPTID.getText,
      (vars zip exps).toMap, (pvars zip progs).toMap,
      Option(ctx.obtain).map(_.accept(this).asInstanceOf[Fact]) )
  }

  override def visitPop(ctx: HilbertParser.PopContext): POP =
    POP(Option(ctx.name).map(_.getText), ctx.id.getText,
      Option(ctx.obtain).map(_.accept(this).asInstanceOf[Fact]))

  override def visitPush(ctx: HilbertParser.PushContext): PUSH =
    PUSH(Option(ctx.name).map(_.getText), ctx.id.getText,
      ctx.form.accept(this).asInstanceOf[Formula],
      Option(ctx.obtain).map(_.accept(this).asInstanceOf[Fact]))

  override def visitTheorem(ctx: HilbertParser.TheoremContext): THM =
    THM(ctx.name.getText,
      if(Option(ctx.fact).isDefined)
           ctx.fact().accept(this).asInstanceOf[Fact]
      else Fact(ctx.formula.accept(this).asInstanceOf[Formula]),
      for(c <- asScalaBuffer(ctx.command())) yield c.accept(this).asInstanceOf[Command] )

  override def visitMp(ctx: HilbertParser.MpContext): MP =
    MP(Option(ctx.name).map(_.getText),
      ctx.inst != null,
      ctx.first.getText, ctx.second.getText,
      Option(ctx.obtain).map(_.accept(this).asInstanceOf[Fact]))

  override def visitGen(ctx: HilbertParser.GenContext): GEN =
    GEN(Option(ctx.name).map(_.getText), ctx.id.getText,
      Option(ctx.program).map(_.accept(this).asInstanceOf[Program])
        .getOrElse(ATOMIC("X")),
      Option(ctx.obtain).map(_.accept(this).asInstanceOf[Fact]))


  override def visitId(ctx: HilbertParser.IdContext): VARIABLE =
    VARIABLE(ctx.ID().getText)

  override def visitNeg(ctx: HilbertParser.NegContext): NEG =
    NEG(ctx.formula().accept(this).asInstanceOf[Formula])

  override def visitImp(ctx: HilbertParser.ImpContext): IMP =
    IMP(ctx.formula(0).accept(this).asInstanceOf[Formula],
      ctx.formula(1).accept(this).asInstanceOf[Formula])

  override def visitParen(ctx: HilbertParser.ParenContext): Formula =
    ctx.formula().accept(this).asInstanceOf[Formula]

  override def visitBox(ctx: HilbertParser.BoxContext): Formula =
    BOX(ctx.program.accept(this).asInstanceOf[Program],
      ctx.formula().accept(this).asInstanceOf[Formula])

  override def visitDiamond(ctx: HilbertParser.DiamondContext): Formula =
    DIAMOND(ctx.program.accept(this).asInstanceOf[Program],
      ctx.formula().accept(this).asInstanceOf[Formula])


  override def visitAtomic(ctx: HilbertParser.AtomicContext) =
    ATOMIC(ctx.ID.getText)

  override def visitParenProgram(ctx: HilbertParser.ParenProgramContext) =
    ctx.program.accept(this)

  override def visitKleene(ctx: HilbertParser.KleeneContext) =
    KLEENE(ctx.program.accept(this).asInstanceOf[Program])

  override def visitChoice(ctx: HilbertParser.ChoiceContext) =
    CHOICE(ctx.fst.accept(this).asInstanceOf[Program],
      ctx.snd.accept(this).asInstanceOf[Program])

  override def visitSeq(ctx: HilbertParser.SeqContext) =
    SEQ(ctx.fst.accept(this).asInstanceOf[Program],
      ctx.snd.accept(this).asInstanceOf[Program])

  override def visitTest(ctx: HilbertParser.TestContext) =
    TEST(ctx.formula.accept(this).asInstanceOf[Formula])


  override def visitFact(ctx: HilbertParser.FactContext): Fact =
    Fact(asScalaBuffer(ctx.premiss).map(_.accept(this).asInstanceOf[Formula]).toSet,
      ctx.concl.accept(this).asInstanceOf[Formula])


  override def aggregateResult(aggregate: AST, nextResult: AST): AST = {
    println(aggregate)
    sys.error("This should not be called!")
  }

}