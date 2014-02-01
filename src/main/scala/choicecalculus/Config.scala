package choicecalculus

import utility.messages.{ raise, Level }
import utility.strings._

import org.rogach.scallop.{ singleArgConverter, ScallopConf }
import org.kiama.util.Emitter
import scala.collection.immutable.Seq

import phases.Phase

/**
 * Trait to require a configuration object of the given type
 *
 * @tparam C The type of the required configuration
 *
 * @see [[Config]]
 * @see [[CompilerConfig]]
 * @see [[ReplConfig]]
 */
trait Configurable[C <: Config] {

  private var _conf: Option[C] = None

  def conf: C = 
    _conf getOrElse(sys error "Config has not been created yet. Be sure to run driver first!")

  def conf_=(c: C): C = { _conf = Some(c); c }
}

/**
 * Configuration specific to the compiler interface
 */
class CompilerConfig(args: Seq[String]) extends ScallopConf(args) with Config {
  
  /**
   * The list of filenames to process (required)
   */
  val input = opt[List[String]](
    name = "input",
    descr = "The filenames to process",
    argName = "FILENAME",
    required = true
  )

  /**
   * The list of filenames to write the results to (optional)
   *
   * If this option is used the number of inputs has to match the number
   * of outputs.
   */
  val output = opt[List[String]]("output", 
    descr = s"""The filenames to write the results to. If the output option is 
               |used please assure to provide the ${"same number".bold} of input 
               |and output arguments.""".stripLinebreaks,
    argName = "FILENAME",
    default = Some(Nil),
    required = false)
}

/**
 * Configuration specific to the REPL interface
 *
 * `afterInit` has to be called, otherwise ScallopConf does not initialize itself.
 * @see https://github.com/scallop/scallop/issues/79
 */
class ReplConfig(args: Seq[String]) extends ScallopConf(args) with Config { afterInit }

/**
 * Allows configuration of the processing pipeline.
 *
 * @see [[messages]]
 */
trait Config { self: ScallopConf =>

  errorMessageHandler = (msg: String) => raise(msg, phase = 'options, filename = "")

  /**
   * The last executed compiler phase (optional)
   * 
   * Using this option will stop the processing after the provided compiler phase.
   * Note: Since "Reader and Parser" are a combined phase, only `Reader` is a
   * reasonable choice.
   *
   * The `Generator` phase will always be executed to output the results.
   */
  val phase = opt[Phase.Value]("phase", 
    argName = "PHASE",
    descr = s"""The ${"compiler phase".bold} to stop afterwards. Can be one of 
               |${Phase.values mkString "|"} """.stripLinebreaks,

    default = Some(Phase.Evaluator),
    required = true)(phaseConverter)

  /**
   * Allows inspecting the tree instead of the pretty printed result
   *
   * Useful alternative to the debug pretty printer (especially within the REPL)
   */
  val showTree = toggle("showtree",
    descrYes = """Shows the tree instead of pretty printing the result""",
    descrNo = """Pretty prints the result and does not show the tree (default)""")

  /**
   * Options to control message reporting
   */
  object messages {

    /**
     * The minium level for messages to be reported (optional)
     */
    val level = opt[Level.Value]("mlevel",
      argName = "LEVEL",
      descr = s"""The minimum ${"error reporting level".bold} for messages to be
                 |reported. Can be one of [${Level.values mkString "|"}]""".stripLinebreaks,
      default = Some(Level.Info),
      required = true)(levelConverter)

    /**
     * The phase a message has been reported in (optional)
     *
     * Note: Only exact matches will pass the filter
     */
    val phase = opt[Phase.Value]("mphase",
      argName = "PHASE",
      descr = s"""This option allows filtering messages by the phase they have
                 |been raised in. See ${"phase".bold} for valid options.""".stripLinebreaks,
      default = None)(phaseConverter)
  }
  // necessary to force initialization
  messages

  private lazy val phaseConverter = singleArgConverter(Phase.withName(_))
  private lazy val levelConverter = singleArgConverter(Level.withName(_))
}