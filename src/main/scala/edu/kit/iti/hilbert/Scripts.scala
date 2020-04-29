package edu.kit.iti.hilbert

import javax.script.{Compilable, CompiledScript, ScriptEngineManager}

import scala.collection.mutable

object Scripts {

  private val scripts : mutable.Map[String, CompiledScript] = mutable.Map()

  private var engine: Compilable = null

  def defineScript(x: SCRIPTDEF): Unit = {
    if(scripts contains x.name)
      throw new RuntimeException(s"Script ${x.name} already defined")

    if(engine == null) {
      engine = new ScriptEngineManager().getEngineByName("nashorn").asInstanceOf[Compilable]
    }

    val strippedCode = x.code.substring(2, x.code.length-2)

    try {
      val compiledScript = engine.compile(strippedCode)
      scripts.put(x.name, compiledScript);
    } catch {
      case ex: Exception => throw new RuntimeException("Error in script code for " + x.name + ": " + ex.getMessage, ex)
    }
  }



  def callScript(x: SCRIPT, interpreter: Interpreter): Unit = {

    val script = scripts.get(x.scriptName)

    if(!script.isDefined) {
      throw new RuntimeException(s"Script ${x.scriptName} not defined")
    }

    val compiledScript = script.get

    val e = compiledScript.getEngine
    e.put("formulas", x.mapFormula)
    e.put("programs", x.mapProgram)
    e.put("facts", Map())
    val interface = new ScriptInterface(new Interpreter)
    e.put("i", interface)
    compiledScript.eval()
    println(interpreter.factMap.get("goal"))
  }

}

class ScriptInterface(private val interpreter: Interpreter) {

  def inst() = {

  }

}
