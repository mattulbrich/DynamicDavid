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
package edu.kit.iti.hilbert

import edu.kit.iti.hilbert.parser.HilbertParsers

import scala.collection.mutable
import scala.io.Source

class Interpreter {

  val factMap : mutable.Map[String, Fact] = mutable.LinkedHashMap()
  val logMap : mutable.Map[String, Command] = mutable.Map()

  def putFact(name: String, fact: Fact, command: Command): Unit = {
    if(factMap contains name)
      throw new RuntimeException("Fact name aready used: " + name)

    factMap.put(name, fact)
    logMap.put(name, command)

    println("Fact " + name + ":")
    println(fact)
  }

  def putFact(name: String, fact: Fact, command: Command, obtained: Option[Fact]): Unit = {

    if(obtained.isDefined)
      Interpreter.checkObtain match {
        case "ignore" => /* do nothing */
        case "warn" => if (fact != obtained.get) {
          println("The obtained formula differs from the script")
          println("observed: " + fact)
          println("expected: " + obtained.get)
        }
        case "strict" => if (fact != obtained.get) {
          println("observed: " + fact)
          println("expected: " + obtained.get)
          throw new RuntimeException("The obtained formula differs from the script")
        }
      }

    putFact(name, fact, command)
  }

  def pop(pop: POP): Unit = {
    val fact = factMap get(pop.id)
    if(fact equals None)
      throw new RuntimeException("Unknown fact " + pop.id)

    val Fact(pr, concl) = fact.get
    concl match {
      case IMP(a, b) => putFact(pop.name.get, Fact(pr + a, b), pop, pop.obtain)
      case _ => throw new RuntimeException("Expecting Implication: " + concl)
    }
  }

  private def containsModality(fact: Fact): Boolean =
    fact.premiss.exists(containsModality) || containsModality(fact.conclusion)

  private def containsModality(formula: Formula) : Boolean = formula match {
    case v: VARIABLE => false
    case b: BOX => true
    case d: DIAMOND => true
    case IMP(a, b) => containsModality(a) || containsModality(b)
    case NEG(a) => containsModality(a)
  }

  def push(push: PUSH): Unit = {
    val fact = factMap get push.id
    if(fact.isEmpty)
      throw new RuntimeException("Unknown fact " + push.id)

    var orgFact = fact.get
    if(containsModality(orgFact))
      throw new RuntimeException("Deduction theorem is only valid w/o dynamic logic.");

    var newSet = orgFact.premiss
    if(!newSet.contains(push.exp)) {
      throw new RuntimeException("This is not a premiss in " + push.id + ": " + push.exp)
    }

    newSet -= push.exp
    putFact(push.name.get, Fact(newSet, IMP(push.exp, orgFact.conclusion)), push, push.obtain)
  }

  def inst(inst: INST): Unit = {

    def instForm(e: Formula) : Formula = e match {
      case vari @ VARIABLE(v) => inst.mapFormula.getOrElse(v, vari)
      case IMP(a, b) => IMP(instForm(a), instForm(b))
      case NEG(a) => NEG(instForm(a));
      case BOX(p, a) => BOX(instProgram(p), instForm(a))
      case DIAMOND(p, a) => DIAMOND(instProgram(p), instForm(a))
    }

    def instProgram(p: Program) : Program = p match {
      case atomic @ ATOMIC(name) => inst.mapProgram.getOrElse(name, atomic)
      case KLEENE(p) => KLEENE(instProgram(p))
      case SEQ(p, q) => SEQ(instProgram(p), instProgram(q))
      case CHOICE(p, q) => CHOICE(instProgram(p), instProgram(q))
      case TEST(f) => TEST(instForm(f))
    }

    val fact = factMap get inst.id
    if(fact.isEmpty)
      throw new RuntimeException("Unknown fact " + inst.id)

    val newFact = Fact(fact.get.premiss map instForm, instForm(fact.get.conclusion))
    putFact(inst.name.get, newFact, inst, inst.obtain)
  }

  def apply(c: Command): Unit = interpret(c)

  def thm(thm: THM): Unit = {
    val subInt = new Interpreter
    subInt.factMap ++= this.factMap
    subInt.clear(CLEAR(None))
    thm.proof foreach subInt.interpret
    subInt.factMap.get("goal") match {
      case None => throw new RuntimeException("Missing 'goal' in proof");
      case Some(f) =>
        if(f != thm.fact) throw new RuntimeException("Goal is not the theorem goal")
        putFact(thm.name, f, thm)
    }

  }

  def modusPonens(mp: MP): Unit = {

    val fst = factMap get mp.fst
    if(fst.isEmpty) {
      throw new RuntimeException("Unknown id: " + mp.fst)
    }

    val snd = factMap get mp.snd
    if(snd.isEmpty) {
      throw new RuntimeException("Unknown id: " + mp.snd)
    }

    val c1 = fst.get.conclusion
    val c2 = snd.get.conclusion
    val p = fst.get.premiss union snd.get.premiss

    c2 match {
      case IMP(a, b) =>
        if(a equals c1) {
          putFact(mp.name.get, Fact(p, b), mp, mp.obtain)
          return
        }
      case _ =>
    }

    c1 match {
      case IMP(a, b) =>
        if(a equals c2) {
          putFact(mp.name.get, Fact(p, b), mp, mp.obtain)
          return
        }
      case _ =>
    }

    throw new RuntimeException("MP does not match")
  }

  private def generalise(gen: GEN): Unit = {
    val fact = factMap get gen.id

    if(fact.isEmpty) {
      throw new RuntimeException("Unknown id: " + gen.id)
    }

    val newFact = Fact(fact.get.premiss, BOX(gen.prog, fact.get.conclusion))
    putFact(gen.name.get, newFact, gen, gen.obtain)
  }

  private def collectCommands(name: String): List[Command] = {
    val cmd = logMap.get(name)
    if(cmd.isEmpty)
      List()
    else
      cmd.get match {
        case x @ POP(_, id, _) => x :: collectCommands(id)
        case x @ PUSH(_, id, _, _) => x :: collectCommands(id)
        case x @ INST(_, id, _, _, _) => x :: collectCommands(id)
        case x @ MP(_, fst, snd, _) => x :: collectCommands(fst) ::: collectCommands(snd)
        case x @ GEN(_, id, _, _) => x :: collectCommands(id)
        case x => List(x)
      }
  }

  def printFact(print: PRINT_FACT): Unit = print.name match {
    case None =>
      for((k,f) <- factMap) {
        println("Fact " + k + ":")
        println("  " + f)
      }
    case Some(name) => factMap get name match {
      case None => println("Unknown entry " + name)
      case Some(fact) => println("Entry " + name + ":")
        println("  " + fact)
        if(print.withProof) {
          val commands = collectCommands(name).reverse
          for(c <- commands) {
            val cc = Command.updateObtain(c, factMap)
            println(Printer.cmd2str(cc))
            println
          }
        }
    }
  }

  def clear(clear: CLEAR): Unit = {
    if(clear.name.exists( _ equals "all" ))  {
      factMap.clear()
      logMap.clear()
    } else if(clear.name.isEmpty) {
      factMap.retain({ case (k: String, _ ) => !k.startsWith("#") })
      logMap.retain({ case (k: String, _ ) => !k.startsWith("#") })
    } else if(factMap contains clear.name.get) {
      factMap.remove(clear.name.get)
      logMap.remove(clear.name.get)
    } else {
      throw new RuntimeException("Unknown fact '" + clear.name.get + "'. Cannot delete.")
    }
  }

  private def help(sub: Option[String]): Unit =
    try {
      Source.fromResource("help_" +
        sub.getOrElse("main") + ".txt").getLines foreach println
    } catch {
      case _: Exception => println("Unknown help topic: " + sub.get)
    }


  def fillWithName(cmd: Command) = {
    def makeNewName: String = {
      var i = 1
      while (factMap contains "#" + i) {
        i = i + 1;
      }
      "#" + i
    }

    def newName(n: Option[String]) = {
      n.orElse(Some(makeNewName))
    }

    cmd match {
      case ASSUME(name, exp) => ASSUME(newName(name), exp)
      case POP(name, id, obtain) => POP(newName(name), id, obtain)
      case PUSH(name, id, exp, obtain) => PUSH(newName(name), id, exp, obtain)
      case INST(name, id, mapFormula, mapProgram, obtain) =>
        INST(newName(name), id, mapFormula, mapProgram, obtain)
      case MP(name, fst, snd, obtain) => MP(newName(name), fst, snd, obtain)
      case GEN(name, id, prog, obtain) => GEN(newName(name), id, prog, obtain)
      case x => x
    }
  }

  def interpret(cmd: Command): Unit = {
    val namedCmd = fillWithName(cmd)

    namedCmd match {
      case LOAD(file, unless) =>
        if(!unless.exists( factMap.contains(_) ))
          HilbertParsers.parseFile(Interpreter.stripQuotes(file)) foreach interpret
      case x @ ASSUME(name, exp) => putFact(name.get, Fact(exp), x)
      case x : POP => pop(x)
      case x : PUSH => push(x)
      case x : INST => inst(x)
      case x : MP => modusPonens(x)
      case x : GEN => generalise(x)
      case x : THM => thm(x)
      case x : PRINT_FACT => printFact(x)
      case x : CLEAR => clear(x)
      case x : SET => Interpreter.setOption(x)
      case HELP(subcommand) => help(subcommand)
      case QUIT() => System.exit(0)
    }
  }
}

object Interpreter {

  /**
    * Error messages will contain stacktraces if set to true
    */
  var verbose = false

  /**
    * Check mode for obtain clauses:
    *   warn ... issue if the obtain clause is wrong
    *   strict . issue an exception -"-
    *   ignore . do nothing -"-
    */
  var checkObtain = "warn"

  /**
    * Set a global option of the prover engine.
    * Currently supported: "verbose" and "chkObtain".
    *
    * @param set take the settings to change from the command
    */
  def setOption(set: SET): Unit = set.property match {
    case "verbose" =>
      if(set.value != "true" && set.value != "false")
        throw new RuntimeException("verbose must be set to true or false")
      verbose = (set.value == "true")

    case "chkObtain" =>
      if(!Set("strict", "ignore", "warn").contains(set.value))
        throw new RuntimeException("chkObtain must be set to strict, ignore or warn")
      checkObtain = set.value;

    case _ => throw new RuntimeException("Unknown property " + set.property)
  }

  def stripQuotes(quoted: String): String =
    quoted.substring(1, quoted.length - 1)

  def main(args: Array[String]): Unit = {

    println("Dynamic David 0.1 - Interactive Hilbert Calculus for PDL")
    println("  see: https://github.com/mattulbrich/DynamicDavid")
    println("  Type 'help.' for instructions.")
    println

    if (args.length > 0) {
      var command: Command = null
      val intr = new Interpreter
      try {
        HilbertParsers.parseFile(args(0)) foreach
          { x => command = x ; intr.interpret(x) }
      } catch {
        case  ex: Exception =>
          // if(verbose)
            ex.printStackTrace()
          println("Error while handling command: " + command)
          println(ex.getMessage)
      }
    } else
      commandLoop
  }

  /**
    * Interactive command loop with a prompt and error
    */
  def commandLoop: Unit = {
    val command = new StringBuilder
    val interpreter = new Interpreter
    val reader = Source.fromInputStream(System.in).bufferedReader()
    while(true) {
      print("> ")
      var line = reader.readLine();

      if(line == null) {
        System.exit(0)
      }

      line = line.trim()
      if(line endsWith ".") {
        command ++= line.substring(0, line.length-1)
        try {
          println
          val cmd = HilbertParsers.parseCommands(command.mkString)
          cmd.foreach(interpreter(_))
        } catch {
          case  ex: Exception =>
            if(verbose)
              ex.printStackTrace()
            println("Error while handling command: " + command)
            println(ex.getMessage)
        }
        command.clear()
        println
      } else {
        command ++= line
        command ++= "\n"
      }
    }
  }
}
