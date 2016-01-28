package com.pavlinic.util.lua

import org.luaj.vm2._
import org.luaj.vm2.compiler._
import org.luaj.vm2.lib.jse._

/**
 * An attempt at making lua scripts more immutable. Each call evaluates everything
 * again
 */

trait FromLua[T] {
  def apply(l: LuaValue): T
}

object SimpleConversions {
  implicit val lua2Int = new FromLua[Int] {
    def apply(l: LuaValue) = l.checkint()
  }
  implicit val lua2String = new FromLua[String] {
    def apply(l: LuaValue) = l.checkstring().tojstring()
  }
  implicit val lua2StringStringTable = new FromLua[Map[String, String]] {
    def apply(l: LuaValue) = {
      val luaTable = l.checktable()
      Map(luaTable.keys().map { lvKey =>
        lvKey.checkstring().tojstring() -> luaTable.get(lvKey).checkstring().tojstring()
      }: _*)
    }
  }
}


object ShapelessConversions {
  import shapeless._
  implicit def luaToRecord[A <: Product](implicit asRecord: Default.AsRecord[A]): FromLua[A] = new FromLua[A] {
      def apply(l: LuaValue) = {
        println(asRecord())
      ???
    }
  }
}

class EvalImpl(initialScripts: List[String] = List()) {

  def eval[V](luaString: String)(implicit conv: FromLua[V]): V = {
    val globals = minimalGlobals
    initialScripts.foreach(s=> globals.load(s).call())
    conv(globals.load(luaString).call())
  }

  def minimalGlobals: Globals = {
    val globals = new Globals
    LoadState.install(globals)
    LuaC.install(globals)
    globals
  }

  def withScript(script: String) = new EvalImpl(initialScripts = initialScripts :+ script)
}

object Eval extends EvalImpl() {

}
