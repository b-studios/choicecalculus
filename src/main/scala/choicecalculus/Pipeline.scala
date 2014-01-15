package choicecalculus

import phases.{ Parser, Reader, Namer, DimensionChecker, Evaluator }

import utility.messages._

import lang.trees.Tree

trait Pipeline extends Parser with Reader with Namer
    with DimensionChecker with Evaluator with phases.namer.SymbolPreservingRewriter {

  /**
   * Processes a given file by running the reader phase
   */
  def processfile(filename: String): Unit = messageScope(filename = filename) {

    // I. Reader phase - always necessary to collect all
    //    dependencies
    val (source, dependencies) = runReader(filename)

    // other phases for dependencies
    for (dependency <- dependencies; tree <- dependency.trees) {
      processDependency(dependency.filename, tree)
    }

    // Usually, there should only be one tree for the main source
    // but we are playing it save here
    val results = for {
      tree <- source.trees
    } yield process(source.filename, tree)

    println(results)

    // Last Phase: Generator
    // runGenerator
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
      runNamer(ast)
      runDimensionChecker(ast)
    }

  def process(filename: String, ast: Tree): Tree =
    messageScope(filename = filename) {
      runNamer(ast)
      runDimensionChecker(ast)
      runEvaluator(ast)
    }
}