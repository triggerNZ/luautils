package com.pavlinic.util.lua
package generic

import org.luaj.vm2._
import shapeless._, record._, syntax.singleton._, ops.record._, ops.hlist._

object FromLua {
  implicit def lua2A[A <: Product, Fields <: HList, Ks <: HList]
  (implicit gen: LabelledGeneric.Aux[A, Fields],
            ks: Keys.Aux[Fields, Ks],
            toList: ToTraversable.Aux[Ks, List, Symbol],
            folder: LeftFolderWithValue.Aux[Ks, Fields, combine.type, LuaValue, Fields],
            iv: InitialValue[A]): FromLua[A] = new FromLua[A] {
    def apply(l: LuaValue): A = {
      val initial = iv()
      val initialRecord: Fields = gen.to(initial)

      val keys: Ks = initialRecord.keys
      val folded: Fields = folder(keys, initialRecord, l)
      gen.from(folded)

      initial
    }
  }

  /** Dependent ternary function type. */
  trait DepFn3[T, U, V] {
    type Out
    def apply(t: T, u: U, v: V): Out
  }

  object combine extends Poly {
    implicit def onlyCase[Fields, K] = use { (cur: Fields, next: K, v: LuaValue) => cur }
  }


  trait LeftFolderWithValue[L <: HList, In, HF, V] extends DepFn3[L, In, V] with Serializable

  object LeftFolderWithValue {
    def apply[L <: HList, In, F, V](implicit folder: LeftFolderWithValue[L, In, F, V]): Aux[L, In, F, V, folder.Out] = folder

    type Aux[L <: HList, In, HF, V, Out0] = LeftFolderWithValue[L, In, HF, V] { type Out = Out0 }

    implicit def hnilLeftFolder[In, HF, V]: Aux[HNil, In , HF, V, In] =
      new LeftFolderWithValue[HNil, In, HF, V] {
        type Out = In
        def apply(l : HNil, in : In, v: V): Out = in
      }

    implicit def hlistLeftFolder[H, T <: HList, In, HF, OutH, V]
    (implicit f : poly.Case3.Aux[HF, In, H, V, OutH],
     ft : LeftFolderWithValue[T, OutH, HF, V]
      ): Aux[H :: T, In, HF, V, ft.Out] =
      new LeftFolderWithValue[H :: T, In, HF, V] {
        type Out = ft.Out
        def apply(l : H :: T, in : In, v: V) : Out = {
          val combinedWithHead = f(in :: l.head :: v :: HNil)
          ft(l.tail, combinedWithHead, v)
        }
      }
  }



}
