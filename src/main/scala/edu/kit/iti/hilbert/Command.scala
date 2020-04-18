package edu.kit.iti.hilbert

sealed abstract class AST

sealed abstract class Command extends AST

sealed abstract class Expression extends AST

case class LOAD(file: String, unless: Option[String]) extends Command
case class ASSUME(name: Option[String], exp: Expression) extends Command
case class INST(name: Option[String], id: String, map: Map[String, Expression]) extends Command
case class PUSH(name: Option[String], id: String, exp: Expression) extends Command
case class POP(name: Option[String], id: String) extends Command
case class MP(name: Option[String], fst: String, snd: String) extends Command
case class THM(name: String, expression: Expression, proof: Seq[Command]) extends Command
case class QUIT() extends Command
case class CLEAR(all: Boolean) extends Command
case class PRINT_FACT(name: Option[String]) extends Command

case class VARIABLE(name: String) extends Expression
case class IMP(premiss: Expression, conclusion: Expression) extends Expression
case class NEG(expression: Expression) extends Expression

case class Fact(premiss: Set[Expression], conclusion: Expression) {
  override def toString: String =
    premiss.map(exp2str).mkString(", ") + " |- " +
      exp2str(conclusion)

  private def exp2str(e:Expression): String = e match {
    case VARIABLE(v) => v
    case IMP(a@IMP(a1,a2), b) => "(" + exp2str(a) + ") -> " + exp2str(b)
    case IMP(a,b) => exp2str(a) + " -> " + exp2str(b)
    case NEG(VARIABLE(v)) => "-" + v
    case NEG(x) => "-(" + exp2str(x) + ")"
  }
}

object Fact {
  def apply(exp: Expression): Fact = Fact(Set(), exp)
}