package com.pavlinic.util.lua

import org.specs2.Specification

import Eval._

import SimpleConversions._
import generic.FromLuaGenericImplicits._

class EvalSpec extends Specification { def is = s2"""
      evaluate an int $intValue
      evaluate an string $stringValue
      evaluate something with functions already installed $scripts
      evaluate tables $tables
      evaluate flat case classes $flatCaseClass
      evaluate flat case classes $nestedCaseClass
      optional values $options
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
    Seq(
      eval[Map[String, String]]("return {x = 'a', y = 'b'}") === Map("x" -> "a", "y" -> "b"),
      eval[Map[String, Int]]("return {x = 1, y = 2}") === Map("x" -> 1, "y" -> 2)
    )
  }

  def options = Seq(
    eval[Option[Int]]("return nil") === None,
    eval[Option[Int]]("return 1") === Some(1)
  )

  def flatCaseClass = {
    case class A(i: Int, s: String, opt: Option[Int])
    Seq(
      eval[A]("return {i = 5, s = 'blah'}") === A(5, "blah", None),
      eval[A]("return {i = 5, s = 'blah', opt = 50}") === A(5, "blah", Some(50))
    )
  }

  def nestedCaseClass = {
     case class A(i: Int, b: B)
     case class B(s: String)
    1 === 1
     eval[A]("return {i = 5, b = {s = 'blah'}}") === A(5, B("blah"))
  }
 }
