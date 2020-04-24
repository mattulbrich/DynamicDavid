package edu.kit.iti.hilbert

case class Config(verbose: Boolean = false,
                  checkObtain: String = "warn",
                  directory: String = ".",
                  file: Option[String] = None,
                  jupyterFile: Option[String] = None)

object Main {

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

      /*opt[String]('j', "jupyter")
        .text("act as jupyter notebook kernel")
        .action((x,c) => c.copy(jupyterFile = Some(x)))*/

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

    // parser.parse returns Option[C]
    parser.parse(args, Config()) match {
      case Some(config) =>

        Interpreter.checkObtain = config.checkObtain
        Interpreter.verbose = config.verbose
        Interpreter.directory = config.directory
       /* if(config.jupyterFile.isDefined)
          JupyterKernel.launch(config.jupyterFile.get)
        else*/
          Interpreter.run(config.file)

      case None =>
        sys.exit(1)
    }
  }

}
