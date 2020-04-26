package edu.kit.iti.hilbert

import edu.kit.iti.hilbert.Interpreter.{commandLoop, interpretFile}

case class Config(verbose: Boolean = false,
                  checkObtain: String = "warn",
                  directory: String = ".",
                  file: Option[String] = None,
                  jupyter: Boolean = false)

object Main {

  def VERSION = "0.1"

  val BANNER: Any = "Dynamic David " + Main.VERSION + " - Interactive Hilbert Calculus for PDL\n" +
    "   see: https://github.com/mattulbrich/DynamicDavid"

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("dyndavid") {
      head("Dynamic Davic", "0.1")

      opt[Unit]('v', "verbose")
        .action((x, c) => c.copy(verbose = true))
        .text("switch on verbose error output")

      opt[String]('d', "directory")
        .action((x, c) => c.copy(directory = x))
        .text("the directory from which load commands are read")

      opt[String]('c', "check-obtain")
        .action((x, c) => c.copy(checkObtain = x))
        .validate(x =>
          if (Set("ignore", "warn", "strict").contains(x)) success
          else failure("check-obtain must be ignore, warn or strict"))
        .text("obtain annotations: ignore, warn, strict (default: warn)")

      opt[Unit]('j', "jupyter")
        .text("act as jupyter notebook kernel")
        .action((x, c) => c.copy(jupyter = true))

      help("help").text("prints this usage text")

      note("")
      arg[String]("<file>")
        .maxOccurs(1)
        .optional()
        .action((x, c) => c.copy(file = Some(x)))
        .text("execute this script file and terminate.")

      note("")
      note("Calling program w/o file argument starts the interactive mode")

    }

    println(BANNER)

    // parser.parse returns Option[C]
    parser.parse(args, Config()) match {
      case Some(config) =>

        Interpreter.checkObtain = config.checkObtain
        Interpreter.verbose = config.verbose
        Interpreter.directory = config.directory
        if (config.file.isDefined)
          Interpreter.interpretFile(config.file.get)
        else if(config.jupyter)
          Interpreter.jupityerLoop
        else
          Interpreter.commandLoop

      case None =>
        sys.exit(1)
    }


  }

}
