/**
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

package edu.kit.iti.hilbert

import scala.collection.mutable

sealed abstract class AST

sealed abstract class Command extends AST
sealed abstract class NamingCommand(name: Option[String]) extends Command {
  def getName = name
}

sealed abstract class Formula extends AST {
  override def toString: String = Printer.exp2str(this)
}

sealed abstract class Program extends AST {
  override def toString: String = Printer.prg2str(this, 0)
}

case class ASSUME(name: Option[String], exp: Formula) extends NamingCommand(name)
case class INST(name: Option[String], id: String,
                mapFormula: Map[String, Formula],
                mapProgram: Map[String, Program], obtain: Option[Fact]) extends NamingCommand(name)
case class PUSH(name: Option[String], id: String, exp: Formula, obtain: Option[Fact]) extends NamingCommand(name)
case class POP(name: Option[String], id: String, obtain: Option[Fact]) extends NamingCommand(name)
case class MP(name: Option[String], fst: String, snd: String, obtain: Option[Fact]) extends NamingCommand(name)
case class GEN(name: Option[String], id: String, prog: Program, obtain: Option[Fact]) extends NamingCommand(name)
case class THM(name: String, fact: Fact, proof: Seq[Command]) extends NamingCommand(Some(name))

case class LOAD(file: String, unless: Option[String]) extends Command
case class QUIT() extends Command
case class CLEAR(name: Option[String]) extends Command
case class PRINT_FACT(name: Option[String], withProof: Boolean) extends Command
case class HELP(name: Option[String]) extends Command
case class SET(property: String, value: String) extends Command

case class VARIABLE(name: String) extends Formula
case class IMP(premiss: Formula, conclusion: Formula) extends Formula
case class NEG(expression: Formula) extends Formula
case class BOX(program: Program, formula: Formula) extends Formula
case class DIAMOND(program: Program, formula: Formula) extends Formula

case class ATOMIC(name: String) extends Program
case class SEQ(fst: Program, snd: Program) extends Program
case class CHOICE(fst: Program, snd: Program) extends Program
case class KLEENE(p: Program) extends Program
case class TEST(f: Formula) extends Program

case class Fact(premiss: Set[Formula], conclusion: Formula) extends AST {
  override def toString: String =
    premiss.map(Printer.exp2str).mkString(", ") + " |- " +
      Printer.exp2str(conclusion)
}

object Fact {
  def apply(exp: Formula): Fact = Fact(Set(), exp)
}

object Printer {
  private def paren(str: String, paren: Boolean): String =
    if(paren) "(" + str + ")" else str

  /*
   *  Kleene binds 50
   *  Seq binds 40/39
   *  Choice binds 30/29
   *  Var, Test is atomic
   */
  def prg2str(p: Program, prec: Int): String = p match {
    case ATOMIC(name) => name
    case KLEENE(p) => prg2str(p, 50) + "*"
    case CHOICE(p, q) => paren(prg2str(p, 29) + " + " + prg2str(q, 30), 30 <= prec)
    case TEST(f) => "?" + exp2str(f)
    case SEQ(p, q) => paren(prg2str(p, 39) + " ; " + prg2str(q, 40), 40 <= prec)
  }

  def exp2str(e:Formula): String = e match {
    case VARIABLE(v) => v
    case IMP(a: IMP, b) => "(" + exp2str(a) + ") -> " + exp2str(b)
    case IMP(a,b) => exp2str(a) + " -> " + exp2str(b)
    case NEG(x: IMP) => "-(" + exp2str(x) + ")"
    case NEG(x) => "-" + exp2str(x)
    case BOX(p, a: IMP) => "[" + prg2str(p,0) + "](" + exp2str(a) + ")"
    case BOX(p, a) => "[" + prg2str(p,0 ) + "]" + exp2str(a)
    case DIAMOND(p, a: IMP) => "<" + ">(" + exp2str(a) + ")"
    case DIAMOND(p, a) => "<" + prg2str(p,0 ) + ">" + exp2str(a)
  }


  def cmd2str(c: Command): String = {
    def obt(o: Option[Fact]) =
      o.map("\n  obtain " + _.toString).getOrElse("")
    def nam(n: Option[String]) =
      n.map(" as " + _).getOrElse("")
    def varMap(mapFormula: Map[String, Formula], mapProgram: Map[String, Program]) =
      ((for((k,v) <- mapFormula.toList) yield k + "=" + exp2str(v))
        :::
      (for((k,v) <- mapProgram.toList) yield "program " + k + "=" + prg2str(v, 0))).mkString(", ")

    c match {
      case POP(name, id, obtain) => s"pop from $id${obt(obtain)}${nam(name)}"
      case PUSH(name, id, exp, obtain) => s"push $exp from $id${obt(obtain)}${nam(name)}"
      case ASSUME(name, exp) => s"assume $exp${nam(name)}"
      case INST(name, id, mapFormula, mapProgram, obtain) =>
        s"inst $id with ${varMap(mapFormula, mapProgram)}" +
          s"${obt(obtain)}${nam(name)}"
      case THM(name, fact, proof) => s"thm $fact as $name (proof omitted)"
      case MP(name, fst, snd, obtain) => s"mp $fst and $snd${obt(obtain)}${nam(name)}"
      case GEN(name, id, prog, obtain) => s"gen $id with $prog${obt(obtain)}${nam(name)}"
      case QUIT() => "quit (error)"
    }
  }

}

object Command {

  def updateObtain(c: Command, overwrite: Fact): Command = c match {
    case POP(name, id, obtain) => POP(name, id, Some(overwrite))
    case PUSH(name, id, exp, obtain) => PUSH(name, id, exp, Some(overwrite))
    case INST(name, id, mapFormula, mapProgram, obtain) =>
      INST(name, id, mapFormula, mapProgram, Some(overwrite))
    case MP(name, fst, snd, obtain) => MP(name, fst, snd, Some(overwrite))
    case GEN(name, id, prog, obtain) => GEN(name, id, prog, Some(overwrite))
    case x => x
  }

  def updateObtain(c: Command, map: mutable.Map[String, Fact]): Command = c match {
    case nc: NamingCommand =>
      if(nc.getName.isEmpty) c
      else {
        val f = map.get(nc.getName.get)
        if(f.isEmpty) c
        else updateObtain(c, f.get)
      }
    case c => c
  }

}