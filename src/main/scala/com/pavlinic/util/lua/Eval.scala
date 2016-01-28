package com.pavlinic.util.lua

import org.luaj.vm2._
import org.luaj.vm2.compiler._
import org.luaj.vm2.lib.jse._

/**
 * An attempt at making lua scripts more immutable. Each call evaluates everything
 * again
 */
class EvalImpl(initialScripts: List[String] = List()) {
  implicit def lua2Int = (l: LuaValue) => l.checkint()
  implicit def lua2String = (l: LuaValue) => l.checkstring().tojstring()
  implicit def lua2StringStringTable = (l: LuaValue) => {
    val luaTable = l.checktable()
    Map(luaTable.keys().map { lvKey =>
      lvKey.checkstring().tojstring() -> luaTable.get(lvKey).checkstring().tojstring()
    }: _*)
  }

  def eval[V](luaString: String)(implicit conv: LuaValue => V): V = {
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
