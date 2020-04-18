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
      ctx.expression().accept(this).asInstanceOf[Expression])

  override def visitInstantiate(ctx: HilbertParser.InstantiateContext): INST = {
    val vars = for(v <- asScalaBuffer(ctx.vars)) yield v.getText
    val exps = for(e <- asScalaBuffer(ctx.exps)) yield e.accept(this).asInstanceOf[Expression]
    INST(Option(ctx.name).map(_.getText), ctx.id.getText,
      (vars zip exps).toMap)
  }

  override def visitCommand(ctx: HilbertParser.CommandContext): Command =
    ctx.children.get(0).accept(this).asInstanceOf[Command]

  override def visitFact(ctx: HilbertParser.FactContext): PRINT_FACT =
    PRINT_FACT(Some(ctx.id.getText))

  override def visitFacts(ctx: HilbertParser.FactsContext): PRINT_FACT =
    PRINT_FACT(None)

  override def visitQuit(ctx: HilbertParser.QuitContext) = QUIT()

  override def visitClear(ctx: HilbertParser.ClearContext) =
    CLEAR(ctx.all != null)

  override def visitLoad(ctx: HilbertParser.LoadContext): LOAD =
    LOAD(ctx.STRING_LIT().getText, Option(ctx.ID).map(_.getText))

  override def visitPop(ctx: HilbertParser.PopContext): POP =
    POP(Option(ctx.name).map(_.getText), ctx.id.getText)

  override def visitPush(ctx: HilbertParser.PushContext): PUSH =
    PUSH(Option(ctx.name).map(_.getText), ctx.id.getText,
      ctx.expression().accept(this).asInstanceOf[Expression])

  override def visitTheorem(ctx: HilbertParser.TheoremContext): THM =
    THM(ctx.name.getText,
      ctx.expression().accept(this).asInstanceOf[Expression],
      for(c <- asScalaBuffer(ctx.command())) yield c.accept(this).asInstanceOf[Command] )

  override def visitMp(ctx: HilbertParser.MpContext): MP =
    MP(Option(ctx.name).map(_.getText),
      ctx.first.getText, ctx.second.getText)

  override def visitId(ctx: HilbertParser.IdContext): VARIABLE =
    VARIABLE(ctx.ID().getText)

  override def visitNeg(ctx: HilbertParser.NegContext): NEG =
    NEG(ctx.expression().accept(this).asInstanceOf[Expression])

  override def visitImp(ctx: HilbertParser.ImpContext): IMP =
    IMP(ctx.expression(0).accept(this).asInstanceOf[Expression],
      ctx.expression(1).accept(this).asInstanceOf[Expression])

  override def visitParen(ctx: HilbertParser.ParenContext): Expression =
    ctx.expression().accept(this).asInstanceOf[Expression]

  override def aggregateResult(aggregate: AST, nextResult: AST): AST =
    sys.error("This should not be called!")

}