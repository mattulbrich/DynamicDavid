package edu.kit.iti.hilbert

import edu.kit.iti.hilbert.parser.HilbertParsers

import scala.collection.{Set, mutable}
import scala.io.Source

class Interpreter {

  val expMap : mutable.Map[String, Fact] = mutable.Map()

  def newName: String = {
    var i = 1
    while (expMap contains "#" + i) {
      i = i + 1;
    }
    "#" + i
  }

  def putFact(name: String, fact: Fact): Unit = {
    if(expMap contains name)
      throw new RuntimeException("Fact name aready used: " + name)

    expMap.put(name, fact)

    println("Adding fact " + name)
    println(fact)
  }

  def pop(name: String, id: String): Unit = {
    val fact = expMap get(id)
    if(fact equals None)
      throw new RuntimeException("Unknown fact " + id)

    val Fact(pr, concl) = fact.get
    concl match {
      case IMP(a, b) => putFact(name, Fact(pr + a, b))
      case _ => throw new RuntimeException("Expecting Implication: " + concl)
    }
  }

  def push(name: String, id: String, exp: Expression): Unit = {
    val fact = expMap get id
    if(fact.isEmpty)
      throw new RuntimeException("Unknown fact " + id)

    var orgFact = fact.get
    var newSet = orgFact.premiss
    if(!newSet.contains(exp)) {
      throw new RuntimeException("This is not a premiss in " + id + ": " + exp)
    }

    newSet -= exp
    putFact(name, Fact(newSet, IMP(exp, orgFact.conclusion)))
  }

  def inst(name: String, id: String, map: Map[String, Expression]): Unit = {

    def ins(e: Expression) : Expression = e match {
      case vari @ VARIABLE(v) => map.getOrElse(v, vari)
      case IMP(a, b) => IMP(ins(a), ins(b))
      case NEG(a) => NEG(ins(a));
    }

    val fact = expMap get id
    if(fact.isEmpty)
      throw new RuntimeException("Unknown fact " + id)

    val newFact = Fact(fact.get.premiss map ins, ins(fact.get.conclusion))
    putFact(name, newFact)
  }

  def apply(c: Command): Unit = interpret(c)

  def thm(name: String, exp: Expression, proof: Seq[Command]): Unit = {
    val subInt = new Interpreter
    subInt.expMap ++= this.expMap
    subInt.clear(false)
    proof foreach subInt.interpret
    subInt.expMap.get("goal") match {
      case None => throw new RuntimeException("Missing 'goal' in proof");
      case Some(Fact(s, e)) =>
        if (s.nonEmpty) throw new RuntimeException("Goal must not have premisses")
        if (!e.equals(exp)) throw new RuntimeException("Goal is not the theorem goal")
        putFact(name, Fact(exp))
    }

  }

  def modusPonens(name: String, fstId: String, sndId: String): Unit = {

    val fst = expMap get fstId
    val snd = expMap get sndId

    if(fst.isEmpty || snd.isEmpty) {
      throw new RuntimeException("Unknown ids in MP");
    }

    val c1 = fst.get.conclusion
    val c2 = snd.get.conclusion
    val p = fst.get.premiss union snd.get.premiss

    c2 match {
      case IMP(a, b) =>
        if(a equals c1) {
          putFact(name, Fact(p, b))
          return
        }
      case _ =>
    }

    c1 match {
      case IMP(a, b) =>
        if(a equals c2) {
          putFact(name, Fact(p, b))
          return
        }
      case _ =>
    }

    throw new RuntimeException("MP does not match");
  }

  def printFact(name: Option[String]): Unit = name match {
    case None =>
      for((k,f) <- expMap) {
        println("Entry " + k + ":")
        println("  " + f)
      }
    case Some(name) => expMap get name match {
      case None => println("Unknown entry " + name)
      case Some(fact) => println("Entry " + name + ":")
        println("  " + fact)
    }
  }

  def clear(all: Boolean): Unit = {
    if(all) {
      expMap.clear()
    } else {
      expMap.retain({ case (k: String, f: Fact) => !k.startsWith("#") })
    }
  }

  def interpret(cmd: Command): Unit = cmd match {
    case LOAD(file, unless) =>
      if(!unless.exists( expMap.contains(_) ))
         HilbertParsers.parseFile(Interpreter.stripQutes(file)) foreach interpret
    case ASSUME(name, exp) => putFact(name.getOrElse(newName), Fact(exp))
    case POP(name, id) => pop(name.getOrElse(newName), id)
    case PUSH(name, id, exp) => push(name.getOrElse(newName), id, exp)
    case INST(name, id, map) => inst(name.getOrElse(newName), id, map)
    case MP(name, fst, snd) => modusPonens(name.getOrElse(newName), fst, snd)
    case THM(name, exp, proof) => thm(name, exp, proof)
    case PRINT_FACT(name) => printFact(name)
    case CLEAR(all) => clear(all)
    case QUIT() => System.exit(0)
  }


}

object Interpreter {

  private def stripQutes(quoted: String): String =
    quoted.substring(1, quoted.length - 1)

  def main(args: Array[String]): Unit =
    if(args.length > 0) {
      val intr = new Interpreter
      HilbertParsers.parseFile(stripQutes(args(0))) foreach intr.interpret
    } else
      commandLoop

  def commandLoop = {
    val command = new StringBuilder
    val interpreter = new Interpreter
    for(line <- Source.fromInputStream(System.in).getLines()) {
      if(line endsWith  ".") {
        command ++= line.substring(0, line.length-1)
        try {
          val cmd = HilbertParsers.parseCommands(command.mkString)
          cmd.foreach(interpreter(_))
        } catch {
          case ex => ex.printStackTrace()
            println("Error while handling command: " + command)
        }
        command.clear()
      } else {
        command ++= line
        command ++= "\n"
      }
    }
  }
}
