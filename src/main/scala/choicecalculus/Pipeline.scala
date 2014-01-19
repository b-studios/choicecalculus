package choicecalculus

import phases.{ Parser, Reader, Namer, DimensionChecker, Evaluator, Generator, Phase }
import Phase._

import utility.messages._

import lang.trees.Tree

trait Pipeline[C <: Config] extends Parser with Reader with Namer
    with DimensionChecker with Evaluator with phases.namer.SymbolPreservingRewriter
    with Generator { self: Configurable[C] =>

  /**
   * Processes a given file by running the reader phase
   */
  def processfile(filename: String): String = messageScope(filename = filename) {


    // I. Reader phase - always necessary to collect all
    //    dependencies
    val (source, dependencies) = runReader(filename)

    // other phases for dependencies
    for (dependency <- dependencies; tree <- dependency.trees) {
      processDependency(dependency.filename, tree)
    }

    // Usually, there should only be one tree for the main source
    // but we are playing it safe here

    val results = for {
      tree <- source.trees
    } yield process(source.filename, tree)

    // Last Phase: Generator
    runGenerator(results.head)
  }

  /**
   * Runs part of the pipeline that is applicable for dependencies
   * (Currently excluding the evaluator and the generator)
   *
   * 1. Namer
   * 2. DimensionChecker
   */
  def processDependency(filename: String, ast: Tree): Unit =
    messageScope(filename = filename) {
      for {
        ast <- Namer process ast
        ast <- DimensionChecker process ast
      } ();
    }

  /**
   * Runs the main pipeline on the given `ast` using `filename` for
   * messages.
   */
  def process(filename: String, ast: Tree): Tree =
    messageScope(filename = filename) {
      (for {
        ast <- Namer process ast
        ast <- DimensionChecker process ast
        ast <- Evaluator process ast
      } yield ast) getOrElse ast
    }

  /**
   * Reports the queued messages by applying the filters provided by
   * commandline options.
   */
  def reportFiltered {
    report { msg => 
      msg.level >= conf.messages.level() && (conf.messages.phase.get match {
        case Some(phase) => msg.phase.name == phase.toString.toLowerCase
        case None => true
      })
    }
  }

  private implicit class PhaseDispatcher(phase: Phase.Value) {

    def process(in: Tree): Option[Tree] = (phase <= conf.phase()) match {
      case true => Some(phase match {
        case Namer => runNamer(in)
        case DimensionChecker => runDimensionChecker(in)
        case Evaluator => runEvaluator(in)
        case _ => raise(s"Phase $phase is not supported as pipeline breakpoint")
      })
      case false => None
    }
  }
}