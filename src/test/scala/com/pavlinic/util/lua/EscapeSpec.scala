package com.pavlinic.util.lua

import org.specs2.Specification

class EscapeSpec extends Specification { def is = s2"""
     escape a safe string $safe
     escape a multiline string $multiline
     escape "nested" comments $nested
  """

  def safe = Escape.escape("hello") === """[[hello]]"""

  def multiline = Escape.escape(
    """Line1
      |Line2""".stripMargin) ===
    """[[Line1
      |Line2]]""".stripMargin

  def nested = Escape.escape(
    "[[Hi]]") === """[=[[[Hi]]]=]""".stripMargin
}
