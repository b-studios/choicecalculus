package choicecalculus
package utility

import scala.Console.{ YELLOW, RED, RESET, WHITE }
import scala.util.parsing.input.{ NoPosition, Positional, Position }  
import scala.util.control.ControlThrowable
import scala.collection.mutable.ListBuffer
import org.kiama.util.{ Emitter, Positioned } 

import scala.language.implicitConversions

import scala.util.DynamicVariable
import java.lang.ThreadLocal

/**
 * The messaging backend.
 *
 * Offers facilities to report messages at various loglevels.
 *
 * Also allows lifting a message to an exception and the other way around
 * to enable simple control flow handling.
 *
 * Message buffering is inspired by kiama's messaging facilities.
 */
object messages {

  object Level extends Enumeration {
    type Level = LogLevel

    case class LogLevel(
        private val _i: Int, 
        private val _name: String, 
        color: String) extends Val(_i, _name) {

      def label: String = color + _name.toLowerCase  + RESET
    }

    val Debug = LogLevel(0, "Debug", RESET)
    val Info  = LogLevel(1, "Info",  WHITE)
    val Warn  = LogLevel(2, "Warn",  YELLOW)
    val Error = LogLevel(3, "Error", RED)

  }
  import Level._

  case class Message(
      level: Level, 
      phase: Symbol,
      filename: String,
      position: Position,
      message: String) {

    /**
     * Allows throwing this message as [[FatalPhaseError]]
     */
    def raise: Nothing = throw FatalPhaseError(phase, filename, position, message)

    override def toString: String =
      formatMessage(level, phase, filename, position, message)
  }


  case class FatalPhaseError(
      phase: Symbol, 
      filename: String,
      position: Position, 
      message: String) extends ControlThrowable {

    /**
     * Allows reporting this error in the message queue
     */
    def report { error(message, phase, filename, position) }

    def reraise: Nothing = throw this

    override def toString = 
      formatMessage(Error, phase, filename, position, message)

  }

  private def formatMessage(
      level: Level, 
      phase: Symbol,
      filename: String,
      position: Position,
      message: String): String = {

    val levelStr = s"[${level.label}]"
    val fileStr = (filename, phase) match {
      case (NoFilename, NoPhase) => ""
      case (NoFilename, _) => phase.name
      case (_, NoPhase) => shortenPath(filename)
      case _ => s"${phase.name} - ${shortenPath(filename)}"
    }

    val posStr = position match {
      case `NoPosition` => ""
      case _ => " (%d.%d)".format(position.line, position.column)
    }

    (fileStr, posStr) match {
      case ("", "") => s"$levelStr $message"
      case _ => s"$levelStr ${fileStr}${posStr}: $message"
    }
}

  val NoPhase = Symbol("NOPHASE")
  val NoFilename = ""

  private var isMuted = new DynamicVariable(false)
  private var currentFilename = new DynamicVariable(NoFilename)
  private var currentPhase = new DynamicVariable(NoPhase)

  /**
   * We have to use ThreadLocal as opposed to InheritedThreadLocal (used by the
   * implementation of DynamicVariable) since messages should not be shared /
   * inherited by threads.
   */
  private val messages = new ThreadLocal[ListBuffer[Message]] {
    override def initialValue = new ListBuffer[Message]()
  }


  /**
   * Don't allow emitting of any messages during the execution of `block`
   *
   * @group Scoped
   */
  def mute[T](block: => T): T = isMuted.withValue(true) { block }

  /**
   * Creates a scope for messages with the given parameters
   *
   * @tparam T the type of the result when executing block
   *
   * @param filename the default filename to set for the scope
   * @param phase the default phase to set for the scope
   * @param block the block to execute within the scope
   *
   * @return the result of executing block within the scope
   *
   * @group Scoped
   *
   * @example {{{
   *   messageScope(phase = 'reader) {
   *     warn("some warning")
   *   }
   *   // => some warning with phase "reader" is emitted
   * }}}
   */
  def messageScope[T](
      filename: String = currentFilename, 
      phase: Symbol = currentPhase)(block: => T): T = 
    currentFilename.withValue(filename) { 
      currentPhase.withValue(phase) {
        block
      }
    }

  /**
   * Executes the given block and returns it's
   * result wrapped as `Right` value
   *
   * If errors ([[FatalPhaseError]]) occured during the execution
   * of the function the error is returned as a `Left` value
   *
   *
   * @param block the block to be executed
   *
   * @return the result of f as Right, Left if errors 
   *         have been reported
   *
   * @group Scoped
   */
  def noErrors[T](block: => T): Either[FatalPhaseError, T] = try {
    Right(block)
  } catch {
    case err: FatalPhaseError => Left(err)
  }

  /**
   * @group message buffering
   */
  def message(
      msg: String,
      level: Level = Info,
      phase: Symbol = currentPhase,
      filename: String = currentFilename,
      position: Position = NoPosition) {

    if (!isMuted) {
      messages += Message(level, phase, filename, position, msg)
    }
  }

  /**
   * @group message buffering
   */
  def debug(
      msg: String,
      phase: Symbol = currentPhase,
      filename: String = currentFilename,
      position: Position = NoPosition) {
    message(msg, Debug, phase, filename, position)
  }

  /**
   * @group message buffering
   */
  def info(
      msg: String,
      phase: Symbol = currentPhase,
      filename: String = currentFilename,
      position: Position = NoPosition) { 
    message(msg, Info, phase, filename, position)
  }

  /**
   * @group message buffering
   */
  def warn(
      msg: String,
      phase: Symbol = currentPhase,
      filename: String = currentFilename,
      position: Position = NoPosition) { 
    message(msg, Warn, phase, filename, position)
  }

  /**
   * @group message buffering
   */
  def error(
      msg: String,
      phase: Symbol = currentPhase,
      filename: String = currentFilename,
      position: Position = NoPosition) { 
    message(msg, Error, phase, filename, position)
  }

  /**
   * @group message buffering
   */
  def raise(
      msg: String,
      phase: Symbol = currentPhase,
      filename: String = currentFilename,
      position: Position = NoPosition): Nothing = {
    throw FatalPhaseError(phase, filename, position, msg)
  }


  /**
   * @group message reporting
   */
  def report(implicit emitter: Emitter) { report(messages, emitter) }

  /**
   * Reports all messages that pass the provided filter
   *
   * @group message reporting
   *
   * @example {{{
   *   report { msg => msg.level > Info && msg.phase == 'reader }
   * }}}
   */
  def report(filter: Message => Boolean)(implicit emitter: Emitter) {
    report(messages.filter(filter), emitter)
  }

  /** 
   * @group message reporting
   */
  def resetMessages() { messages.clear }

  /**
   * <strong>For debugging purpose only</strong>
   *
   * Allows inspecting the reported messages in order to check whether
   * a message has been reported as expected.
   *
   * @group debugging
   */
  def hasBeenReported(filter: Message => Boolean): Boolean = 
    messages.exists(filter)

  private def report(msgs: Seq[Message], emitter: Emitter) {
    for (msg <- sortMessages(msgs))
      emitter emitln msg
  }

  /**
   * First group by file, then by phase, then sort by pos
   */
  private def sortMessages(msgs: Seq[Message]): Seq[Message] =
    msgs
      // group by file
      .groupBy { _.filename }
      .map { case (_, fmsgs) =>

        // group by phase
        fmsgs.groupBy { _.phase }
          .map { case (_, pmsgs) => 

            // sort by position
            pmsgs.sortWith { _.position < _.position }
          }.flatten
      }
      .flatten
      .toSeq

  private val basepath = (new java.io.File(".")).getCanonicalPath + "/"
  private def shortenPath(filename: String): String = 
    if (filename startsWith basepath) {
      filename replaceFirst(basepath, "")
    } else {
      filename
    }

  private implicit def getValue[T](v: DynamicVariable[T]): T = v.value
  private implicit def getValue[T](v: ThreadLocal[T]): T = v.get

  /**
   * @group implicits
   */
  implicit def positioned2position(p: Positioned): Position = p.start
  
  /**
   * @group implicits
   */
  implicit def positional2position(p: Positional): Position = p.pos

  /**
   * @group implicits
   */
  implicit def defaultEmitter: Emitter = new Emitter
}