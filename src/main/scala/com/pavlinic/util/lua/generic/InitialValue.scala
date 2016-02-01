package com.pavlinic.util.lua
package generic

import shapeless._

trait InitialValue[A] extends DepFn0 {
  type Out = A
}

object InitialValue {
  def apply[A](implicit iva: InitialValue[A]) = iva

  implicit val ivInt = new InitialValue[Int] {
    def apply = 0
  }
  implicit val ivString = new InitialValue[String] {
    def apply = ""  //null???
  }

  implicit val ivHnil = new InitialValue[HNil] {
    def apply = HNil
  }

  implicit def ivHcons[H, T<: HList]
  (implicit ivHead: InitialValue[H], ivTail: InitialValue[T]):
  InitialValue[H :: T] = new InitialValue[H :: T] {
    def apply = ivHead() :: ivTail()
  }

  implicit def ivProd[P <: Product, HL <: HList]
  (implicit gen: Generic.Aux[P, HL], ivHl: InitialValue[HL]):
  InitialValue[P] = new InitialValue[P] {
    def apply = gen.from(ivHl())
  }

}