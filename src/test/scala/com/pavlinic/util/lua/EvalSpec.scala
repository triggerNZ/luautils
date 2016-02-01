package com.pavlinic.util.lua

import org.specs2.Specification

import Eval._

import SimpleConversions._

class EvalSpec extends Specification { def is = s2"""
      evaluate an int $intValue
      evaluate an string $stringValue
      evaluate something with functions already installed $scripts
      evaluate tables $tables
      evaluate flat case classes $flatCaseClass
      evaluate flat case classes $nestedCaseClass
   """

   def intValue = {
     eval[Int]("return 2 + 2") === 4
   }

  def stringValue = {
    eval[String]("return 'apple'") === "apple"
  }

  def scripts = {
    withScript(
      """
        |   function add (a, b)
        |      local sum = a + b
        |      return sum
        |    end
      """.stripMargin).eval[Int]("return add(3,4)") === 7
  }

  def tables = {
    eval[Map[String, String]]("return {x = 'a', y = 'b'}") === Map("x" -> "a", "y" -> "b")
  }

  def flatCaseClass = {
    import generic.FromLua._

    case class A(i: Int, s: String)
    eval[A]("return {i = 5, s = 'blah'}") === A(5, "blah")
  }

  def nestedCaseClass = {
    import generic.FromLua._

     case class A(i: Int, b: B)
     case class B(s: String)
    1 === 1
     eval[A]("return {i = 5, b = {s = 'blah'}}") === A(5, B("blah"))
  }
 }
