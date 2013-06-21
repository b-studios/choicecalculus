name in ThisBuild := "choicecalculus"

organization in ThisBuild := "de.unimarburg.ps"

version in ThisBuild := "0.1-SNAPSHOT"

scalaVersion in ThisBuild := "2.10.2"

resolvers in ThisBuild ++= Seq (
    Resolver.sonatypeRepo ("releases"),
    Resolver.sonatypeRepo ("snapshots")
)

libraryDependencies in ThisBuild ++= Seq (
    "com.googlecode.kiama" %% "kiama" % "1.5.0"
)

logLevel in ThisBuild := Level.Info

mainClass in ThisBuild := Some("choicecalculus.interpreter.CommandLine")

/**
 * When entering the console, these basic commands are executed:
 */
initialCommands in console := """
    import choicecalculus._
    object parser extends choicecalculus.parser.ChoiceCalculusParser with choicecalculus.parser.JavaScriptParser
""".stripMargin

/**
 * sbt shell prompt
 */
shellPrompt <<= (name, version) { (_, v) =>
     _ => "choicecalc (" + v + ")> "
}
