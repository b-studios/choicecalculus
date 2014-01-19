package choicecalculus
package phases
package generator

import lang.trees._
import lang.PrettyPrinter

import utility.strings.StringOps

import org.kiama.attribution.Attribution.IdMemoised
import scala.collection.JavaConverters._

import collection.mutable

import scala.util.parsing.json.{ JSONObject, JSONArray }
import org.kiama.rewriting.Rewriter.{ everywheretd, query }
import org.kiama.util.{ StringEmitter }

import utility.messages.report

trait DebugGenerator extends Generator { self: Namer with DimensionChecker =>

  val prettyprinter: PrettyPrinter with DebugPrettyPrinter

  def runGenerator(tree: Tree): String = {
    val content = prettyprinter.pretty(prettyprinter.toDoc(tree), 140)
    val msgs = new StringEmitter match {
      case emitter => report(emitter); replaceColors(emitter.result);
    }
    template(content.escape.safeUnescape, msgs, dimensionMap(tree))
  }

  trait DebugPrettyPrinter extends PrettyPrinter {

    private def idAttr(e: Tree): Doc = 
      text(s"<span data-id='${ref(e)}'>".safe) <> super.toDoc(e) <> text("</span>".safe)

    private def bound(e: Tree): Doc = 
      text(s"<span data-ref='${ref((e->symbol).definition)}' data-id='${ref(e)}'>".safe) <>
        super.toDoc(e) <> 
      text("</span>".safe)

    override def toDoc(e: Tree): Doc = e match {
      case id: Identifier => bound(id)
      case choice: Choice => bound(choice)
      case other => idAttr(other)
    }
  }

  private def dimensionMap(tree: Tree): JSONObject = {

    val info: mutable.Map[String, Any] = mutable.Map()

    val collectInfo = query {
      case t: Tree with Product => info update(ref(t), obj(
        "constructor" -> constructorName(t),
        "position" -> obj(
          "start" -> t.start.toString,
          "finish" -> t.finish.toString
        ),

        // TODO ask for dimensioning.hasBeenComputedAt(t)
        // before printing, since we might have skipped the dimensioning phase
        "dimensioning" -> printDimensionGraph(t->dimensioning)
      ))
    }

    everywheretd(collectInfo)(tree)

    // has to be rebuilt, since it is mutable
    new JSONObject(info.toMap)
  }

  private def constructorName(p: Product) = 
    s"""${p.productPrefix}(${(0 until p.productArity) map(_ => "…") mkString (", ")})"""

  /**
   * Helper method to create json objects
   *
   * @example {{{
   *   obj("foo" -> 42).toString()
   *   //=> { "foo": 42 }
   * }}}
   */
  private def obj(attrs: (String, Any)*): JSONObject =
    new JSONObject(attrs.toMap)

  /**
   * Helper method to create json arrays
   *
   * @example {{{
   *   arr(1,2,3,4).toString()
   *   //=> [1, 2 ,3 ,4 ]
   * }}}
   */
  private def arr(els: Any*): JSONArray = 
    new JSONArray(els.toList)

  /**
   * Helper method to replace ANSI colors in string by
   * a span tag. Can only be used if a color is immediately
   * followed by a RESET.
   */
  private def replaceColors(msgs: String): String =
    Map(
      "\\033\\[33m" -> "<span class=\'yellow\'>",
      "\\033\\[31m" -> "<span class=\'red\'>",
      "\\033\\[37m" -> "<span class=\'white\'>",
      "\\033\\[0m"  -> "</span>"
    ).foldLeft(msgs) { case (str, (color, tag)) => str.replaceAll(color, tag) }

  private def printDimensionGraph(g: DependencyGraph): JSONArray = {
    new JSONArray(g.dims.map(printDependentDimension(_)).toList)
  }

  private def printDependentDimension(d: DependentDimension): JSONObject = {
    obj(
      "name" -> d.dim.name.name,
      "ref" -> ref(d.dim),
      "dependencies" -> new JSONArray(d.dependsOn.map(printDependency(_)).toList)
    )
  }

  private def printDependency(d: Dependency): JSONObject =
    obj(
      "selection" -> s"${d.dim.name.name}.${d.tag.name}",
      "ref" -> ref(d.dim)
    )

  /**
   * Creates a unique identifier for the given reference
   */
  private def ref(t: AnyRef): String = 
    s"id-${System.identityHashCode(t).toString}"

  private def template(body: String, messages: String, nodeinfo: JSONObject) = s"""
    <!DOCTYPE html>
    <html ng-app>
      <head>
        <meta charset="utf-8"/>
        <title>Choice Calculus Explorer</title>
        <link href='http://fonts.googleapis.com/css?family=Cabin:400,500' rel='stylesheet' type='text/css'>
        <script src="http://code.jquery.com/jquery-1.10.1.min.js"></script>
        <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.2.9/angular.min.js"></script>
        <script>$jsTemplate</script>
        <style>$cssTemplate</style>
      </head>
      <body class="layout">
        <div class="content" ng-controller="NodeInfoCtrl">
          <code><pre>$body</pre></code>
          <aside id="info">
            <h2>Node Information</h2>
            <dl>
              <span ng-repeat="info in infos">
                <dt>{{ info.name }}</dt>
                <dd>{{ info.data }}</dd>
              </span>
            </dl>

            <h2>Dimensions</h2>
            <ul>
              <li ng-repeat="dimension in dimensioning">
                <span class="ref" ng-click="show(dimension.ref)">{{dimension.name}}</span>
                <span ng-if="dimension.dependencies.length > 0">depends on</span>
                <ul>
                  <li ng-repeat="dependency in dimension.dependencies">
                    <span class="ref" ng-click="show(dependency.ref)">{{dependency.selection}}</span>
                  </li>
                </ul>
              </li>
            </ul>
          </aside>
        </div>
        <footer id="messages">
          <pre>$messages</pre>
        </footer>
        <script>var NODE_INFO = $nodeinfo;</script>
      </body>
    </html>"""

  private val jsTemplate = """
    function NodeInfoCtrl($scope) {

      var $dom = angular.element('.content > code'),
          $nodes = angular.element('.content > code span[data-id]');

      function clearHighlights() {
        $dom.removeClass('selection-active')
        $nodes
          .removeClass('highlight')
          .removeClass('scope-highlight');
      }

      function highlight(ref) {
        var $node = $dom.find('[data-id=' + ref + ']'),
            $ref  = $dom.find('[data-id=' + $node.attr('data-ref') + ']');

        $dom.addClass('selection-active');
        $node.addClass('highlight');
        $ref.addClass('scope-highlight');
      }

      function showDetails(ref) {
        var infos = NODE_INFO[ref];

        $scope.infos = [{
          name: 'Constructor',
          data: infos.constructor
        }, {
          name: 'Position',
          data: infos.position.start + ' to ' + infos.position.finish
        }];

        $scope.dimensioning = infos.dimensioning;
      }

      function hideDetails() {
        $scope.dimensioning = [];
        $scope.infos = [];
      }

      $nodes.on('click', function (_) {
        var id = $(this).data('id');
        $scope.$apply(function () {
          $scope.show(id);
        });
        return false;
      });

      $dom.on('click', function (_) {
        $scope.$apply(function () {
          clearHighlights();
          hideDetails();
        });
      });

      $scope.show = function(ref) {
        clearHighlights();
        highlight(ref);
        showDetails(ref);
      }
    }"""

  private val cssTemplate = """
    /* RESET >>> */
    html, body, div, span, applet, object, iframe, h1, h2, h3, h4, h5, h6, p,
    blockquote, pre, a, abbr, acronym, address, big, cite, code, del, dfn, em,
    img, ins, kbd, q, s, samp, small, strike, strong, sub, sup, tt, var, b, u,
    i, center, dl, dt, dd, ol, ul, li, fieldset, form, label, legend, table,
    caption, tbody, tfoot, thead, tr, th, td, article, aside, canvas, details,
    embed, figure, figcaption, footer, header, hgroup, menu, nav, output, ruby,
    section, summary, time, mark, audio, video {
      margin: 0; padding: 0; border: 0;
      font-size: 100%; font: inherit; vertical-align: baseline;
    }
    article, aside, details, figcaption, figure, footer, header, hgroup, menu,
    nav, section { 
      display: block;
    }
    ol, ul {
      list-style: none;
    }
    blockquote, q {
      quotes: none;
    }
    blockquote:before, blockquote:after, q:before, q:after {
      content: ''; content: none;
    }
    table {
      border-collapse: collapse; border-spacing: 0;
    }
    /* <<< RESET */

    body {
      margin: 0;
      min-width: 100%;
      background: #f1f1f1;
      font-family: sans-serif;
      font-size: 10pt;
      line-height: 1.1em;
      cursor: default;
    }

    .layout, .content {
      display: flex;
      flex-direction: column;
      flex: 1;
    }

    .layout {
      min-height: 100vh;
    }

    .content {
      flex-direction: row;      
    }

    .content > code, footer, aside {
      padding: 1em;
    }

    .content > code, footer {
      font-family: consolas, monospace;
    }

    .content > code {
      width: 100%;
      line-height: 1.2em;
    }

    .content > code.selection-active {
      color: rgba(0, 0, 0, 0.4);
    }

    .content > code .highlight {
      color: rgba(0, 0, 0, 1);
      background: rgb(230, 230, 234);
      padding: 0.2em;
    }

    .content > code .scope-highlight {
      color: rgba(0, 0, 0, 0.8);
    }

    aside#info {
      flex: 0 0 24em;
      background: #e2e7ed;
      border-left: 1px solid #bdbdbd;
      text-shadow: 0px 1px #fff;
      font-size: 0.8em;
      line-height: 1.5em;
    }

    aside#info dl dt {
      margin-top: 0.5em;
      font-weight: bold;
    }

    aside#info dl dd {
      margin-left: 2em;
    }

    aside#info > ul > li {
      margin-top: 0.5em;
      margin-bottom: 0.5em;
    }

    aside#info ul ul {
      margin-left: 1em;
    }

    aside#info span.ref {
      border-bottom: 1px solid #bdbdbd;
      cursor: pointer;
    }

    h1, h2, h3, h4, h5, h6 {
      font-size: 1.2em;
      margin-top: 0.5em;
      font-family: Cabin, sans-serif;
    }

    aside#info strong, h1, h2, h3, h4, h5, h6 {
      color: #8392ac;
      font-weight: 500;
    }

    footer {
      color: #3f4145;
      border-top: 1px solid #acacac;
      background: rgb(230,230,230);
    }

    footer .yellow { color: #d9bc00;}
    footer .red { color: #d90000; }
    footer .white { color: #ffffff; }"""

}