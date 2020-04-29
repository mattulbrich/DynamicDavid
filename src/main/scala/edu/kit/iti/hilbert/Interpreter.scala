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

import java.io._
import java.nio.file.{NoSuchFileException, Paths}

import edu.kit.iti.hilbert.parser.HilbertParsers
import org.jline.reader.{EndOfFileException, LineReaderBuilder, UserInterruptException}

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

class Interpreter {

  val factMap : mutable.Map[String, Fact] = mutable.LinkedHashMap()
  val logMap : mutable.Map[String, Command] = mutable.Map()

  var silent = false;

  def apply(c: Command): Unit = interpret(c)

  def interpret(cmd: Command): Unit = {
    val namedCmd = fillWithName(cmd)

    namedCmd match {
      case LOAD(file, unless) =>
        if(!unless.exists( factMap.contains(_) ))
          Interpreter.parseFile(Interpreter.stripQuotes(file)) foreach interpret
      case x @ ASSUME(name, fact) => putFact(name.get, fact, x)
      case x : POP => pop(x)
      case x : PUSH => push(x)
      case x : INST => inst(x)
      case x : MP => modusPonens(x)
      case x : GEN => generalise(x)
      case x : THM => thm(x)
      case x : PRINT_FACT => printFact(x)
      case x : CLEAR => clear(x)
      case x : SET => setOption(x)
      case HELP(subcommand) => help(subcommand)
      case QUIT() => System.exit(0)
    }
  }

  def putFact(name: String, fact: Fact, command: Command): Unit = {
    if(factMap contains name)
      throw new RuntimeException("Fact name aready used: " + name)

    factMap.put(name, fact)
    logMap.put(name, command)

    if(!silent) {
      println("Fact " + name + ":")
      println(fact)
    }
  }

  /**
    * Add a fact to the fact base and check it against the "obtained" annotation.
    *
    * Interpreter.checkObtain is queried to decide how to react to failure.
    *
    * @param name the name of the fact to be added
    * @param fact the actual fact
    * @param command the command responsible for the new fact
    * @param obtained the obtained annotation to check
    */

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


  def instantiate(e: Formula, mapFormula: Map[String, Formula], mapProg: Map[String, Program]) = {

    def instForm(e: Formula) : Formula = e match {
      case vari @ VARIABLE(v) => mapFormula.getOrElse(v, vari)
      case IMP(a, b) => IMP(instForm(a), instForm(b))
      case NEG(a) => NEG(instForm(a));
      case BOX(p, a) => BOX(instProgram(p), instForm(a))
      case DIAMOND(p, a) => DIAMOND(instProgram(p), instForm(a))
    }

    def instProgram(p: Program) : Program = p match {
      case atomic @ ATOMIC(name) => mapProg.getOrElse(name, atomic)
      case KLEENE(p) => KLEENE(instProgram(p))
      case SEQ(p, q) => SEQ(instProgram(p), instProgram(q))
      case CHOICE(p, q) => CHOICE(instProgram(p), instProgram(q))
      case TEST(f) => TEST(instForm(f))
    }

    instForm(e)
  }


  def inst(inst: INST): Unit = {

    val fact = factMap get inst.id
    if(fact.isEmpty)
      throw new RuntimeException("Unknown fact " + inst.id)

    def I(f:Formula) = instantiate(f, inst.mapFormula, inst.mapProgram)

    val newFact = Fact(fact.get.premiss map I, I(fact.get.conclusion))
    putFact(inst.name.get, newFact, inst, inst.obtain)
  }

  def thm(thm: THM): Unit = {
    val subInt = new Interpreter
    // by default do not echo in thms.
    subInt.silent = true
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

  private def matchFormula(a: Formula, b: Formula) = {

    val mapFormula: mutable.Map[String, Formula] = mutable.Map()
    val mapProgram: mutable.Map[String, Program] = mutable.Map()

    def mf(a: Formula, b: Formula): Unit =
      if (a != b) (a, b) match {
        case (VARIABLE(n), _) =>
          if (mapFormula.contains(n)) mf(mapFormula.get(n).get, b)
          else mapFormula.put(n, b)
        case (IMP(x1, y1), IMP(x2, y2)) =>
          mf(x1, x2)
          mf(y1, y2)
        case (BOX(p1, f1), BOX(p2, f2)) =>
          mp(p1, p2)
          mf(f1, f2)
        case (DIAMOND(p1, f1), DIAMOND(p2, f2)) =>
          mp(p1, p2)
          mf(f1, f2)
        case (NEG(f1), NEG(f2)) =>
          mf(f1, f2)
        case _ => throw MatchingException()
      }

    def mp(p: Program, q: Program): Unit =
      if (p != q) (p, q) match {
        case (a@ATOMIC(n), _) =>
          if (mapProgram contains n) mp(mapProgram.get(n).get, q)
          else mapProgram.put(n, q)
        case (TEST(f1), TEST(f2)) =>
          mf(f1, f2)
        case (CHOICE(a1, b1), CHOICE(a2, b2)) =>
          mp(a1, a2)
          mp(b1, b2)
        case (SEQ(a1, b1), SEQ(a2, b2)) =>
          mp(a1, a2)
          mp(b1, b2)
        case (KLEENE(p1), KLEENE(p2)) =>
          mp(p1, p2)
        case _ => throw MatchingException()
      }

    try {
      mf(a, b)
      Some(mapFormula.toMap, mapProgram.toMap)
    } catch {
      case _: MatchingException => None
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

    c1 match {
      case IMP(a, b) =>
        if(mp.inst) {
          val m = matchFormula(a, c2) : Option[(Map[String, Formula], Map[String, Program])]
          if(m.isDefined) {
            def I(f:Formula) = instantiate(f, m.get._1, m.get._2)
            val instP = (fst.get.premiss map I) union snd.get.premiss
            putFact(mp.name.get, Fact(instP, I(b)), mp, mp.obtain)
            return
          }
        } else {
          if (a equals c2) {
            putFact(mp.name.get, Fact(p, b), mp, mp.obtain)
            return
          }
        }
      case _ =>
    }

    c2 match {
      case IMP(a, b) =>
        if(a equals c1) {
          println(s"Deprecation warning: ${mp.name.get}. mp should first take the implication! This direction may be removed soon.")
          putFact(mp.name.get, Fact(p, b), mp, mp.obtain)
          return
        }
      case _ =>
    }


    //if(Interpreter.verbose) {
      println("  " + mp.fst + ": " + fst.get)
      println("  " + mp.snd + ": " + snd.get)
    //}
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
        case x @ MP(_, _, fst, snd, _) => x :: collectCommands(fst) ::: collectCommands(snd)
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

  /**
    * Set a global option of the prover engine.
    * Currently supported: "verbose" and "chkObtain".
    *
    * @param set take the settings to change from the command
    */
  def setOption(set: SET): Unit = set.property match {
    case "verbose" =>
      try {
        Interpreter.verbose = set.value.toBoolean
      } catch {
        case ex:Exception =>
          throw new RuntimeException("verbose must be set to true or false", ex)
      }

    case "silent" =>
      try {
        this.silent = set.value.toBoolean
      } catch {
        case ex:Exception =>
          throw new RuntimeException("silent must be set to true or false", ex)
      }

    case "chkObtain" =>
      if(!Set("strict", "ignore", "warn").contains(set.value))
        throw new RuntimeException("chkObtain must be set to strict, ignore or warn")
      Interpreter.checkObtain = set.value;

    case _ => throw new RuntimeException("Unknown property " + set.property)
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
      case MP(name, inst, fst, snd, obtain) => MP(newName(name), inst, fst, snd, obtain)
      case GEN(name, id, prog, obtain) => GEN(newName(name), id, prog, obtain)
      case x => x
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

  var directory = "."

  val regex_backslash = new Regex("""\\(.)""")

  def stripQuotes(quoted: String): String =
    quoted.substring(1, quoted.length - 1)

  def parseFile(str: String) = {
    val filename = str
    val file = Paths.get(Interpreter.directory, filename)
    HilbertParsers.parseFile(file.toAbsolutePath.toString)
  }

  /**
    * interpret a single file.
    *
    * @param file file name
    */
  def interpretFile(file: String) = {
    var command: Command = null
    val intr = new Interpreter

    val content = Source.fromFile(file).mkString;

    println

    val res = doCommand(content, intr)

    sys.exit(if(res) 0 else 1)

  }

  /**
    * Interactive command loop with a simple prompt and error reporting
    */
  def commandLoop: Unit = {
    val command = new StringBuilder
    val interpreter = new Interpreter
    val reader = Source.fromInputStream(System.in).bufferedReader()
    println("  Type 'help.' for instructions.")
    println
    while(true) {
      print("> ")
      var line = reader.readLine();

      if(line == null) {
        System.exit(0)
      }

      line = line.trim()
      if(line endsWith ".") {
        command ++= line.substring(0, line.length-1)
        println
        doCommand(command, interpreter)
        command.clear()
        println
      } else {
        command ++= line
        command ++= "\n"
      }
    }
  }

  /**
    * Interactive command loop with a prompt and error
    */
  def commandLoopJLine: Unit = {
    val command = new StringBuilder
    val interpreter = new Interpreter
    val reader = LineReaderBuilder.builder().build();
    println("  Type 'help.' for instructions.")
    println
    while(true) {
      try {
        var line = reader.readLine("> ").trim()
        if (line endsWith ".") {
          command ++= line.substring(0, line.length - 1)
          println
          doCommand(command, interpreter)
          command.clear()
          println
        } else {
          command ++= line
          command ++= "\n"
        }
      } catch {
        case ex: UserInterruptException => ex.printStackTrace()
        case ex: EndOfFileException => return
      }
    }
  }


  /**
    * Command loop for a jupyter server
    */
  def jupityerLoop: Unit = {
    val interpreter = new Interpreter
    val reader = Source.fromInputStream(System.in).bufferedReader()
    val byteArray = new ByteArrayOutputStream
    val print = new PrintStream(byteArray)

    println
    while(true) {
      var line = reader.readLine

      if(line == null) {
        System.exit(0)
      }

      val command = regex_backslash.replaceAllIn(line.trim, m => m.group(1) match {
        case "n" => "\n"
        case other => other
      })

      doCommand(command, interpreter, print)

      val result = byteArray.toByteArray.mkString.
        replace("\\", "\\\\").
        replace("\n", "\\n");
      println(result)

      byteArray.reset()
    }
  }

  private def doCommand(command: Any, interpreter: Interpreter, out: PrintStream = System.out): Boolean = {
    try {
      val cmd = HilbertParsers.parseCommands(command.toString)
      cmd.foreach(interpreter(_))
      return true
    } catch {
      case ex: NoSuchFileException =>
        // Annoying special casing
        if (verbose)
          ex.printStackTrace(out)
        out.println("Error while handling command: " + command)
        out.println("File not found: " + ex.getMessage)
        return false
      case ex: Exception =>
        if (verbose)
          ex.printStackTrace(out)
        out.println("Error while handling command: " + command)
        out.println(ex.getMessage)
        return false
    }
  }
}

case class MatchingException() extends Exception
