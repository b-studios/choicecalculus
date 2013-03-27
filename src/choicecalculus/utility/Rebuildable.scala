package choicecalculus
package utility

import scala.collection.mutable.WeakHashMap


trait Rebuildable {
  def rebuild[T](children: Object*):T = Rebuilder.rebuild[T](this, children:_*) 
}
object Rebuilder {
  
  // Here we use a trick from Kiama to rebuild the hostlanguage elements without knowing them
  protected val consCache = new WeakHashMap[java.lang.Class[_], java.lang.reflect.Constructor[_]]
  
  def rebuild[T](obj: Object, children: Object*):T = {
    val klass = obj.getClass
    val constructor = consCache.getOrElseUpdate (klass, (klass.getConstructors())(0))
    constructor.newInstance(children:_*).asInstanceOf[T]
  } 
}