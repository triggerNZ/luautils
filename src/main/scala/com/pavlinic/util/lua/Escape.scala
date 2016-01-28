package com.pavlinic.util.lua

object Escape {
  /**
   * Escapes the given String into a valid lua string literal. Transliterated
   * from Lua example in Programming in Lua 3rd Edition, Roberto Ierusalimschy

  function quote (s)
     -- find maximum length of sequences of equal signs
     local n = -1
     for w in string.gmatch(s, "]=*]") do
       n = math.max(n, #w - 2)   -- -2 to remove the ']'s
     end
     -- produce a string with 'n' plus one equal signs
     local eq = string.rep("=", n + 1)
     -- build quoted string
     return string.format(" [%s[\n%s]%s] ", eq, s, eq)
   end

   */
  def escape(s: String): String = {
    //find maximum length of sequences of equal signs
    var n = -1
    val pattern = "]=*]".r
    pattern.findAllIn(s).foreach { w =>
      n = Math.max(n, w.length - 2) //-2 to remove the ']'s
    }
    //produce a string with 'n' plus one equal signs
    val eq = "=" * (n + 1)
    // build quoted string
    "[%s[%s]%s]".format(eq, s, eq)
  }
}
