package com.pavlinic.util.lua
package generic

import org.luaj.vm2._
import shapeless._, record._, syntax.singleton._, ops.record._, ops.hlist._
import shapeless.labelled.{FieldBuilder, FieldType, KeyTag}

object FromLuaGenericImplicits {
  implicit def lua2Option[T : FromLua] = new FromLua[Option[T]] {
    def apply(l : LuaValue) = {
      val flt = FromLua[T]
      if (l.isnil()) None else Some(flt(l))
    }
  }

  implicit def lua2StringValueTable[V : FromLua] = new FromLua[Map[String, V]] {
    def apply(l: LuaValue) = {
      val luaTable = l.checktable()
      Map(luaTable.keys().map { lvKey =>
        val flv = implicitly[FromLua[V]]
        lvKey.checkstring().tojstring() -> flv(luaTable.get(lvKey))
      }: _*)
    }
  }

  implicit def lua2A[A <: Product, Rec <: HList, Fs <: HList]
  (implicit gen: LabelledGeneric.Aux[A, Rec],
            fs: Fields.Aux[Rec, Fs],
            folder: LeftFolderWithValue.Aux[Fs, Rec, combine.type, LuaValue, Rec],
            iv: InitialValue[A]): FromLua[A] = new FromLua[A] {
    def apply(l: LuaValue): A = {
      val initial = iv()
      val initialRecord: Rec = gen.to(initial)

      val fields: Fs = initialRecord.fields
      val folded: Rec = folder(fields, initialRecord, l)
      gen.from(folded)
    }
  }

  /** Dependent ternary function type. */
  trait DepFn3[T, U, V] {
    type Out
    def apply(t: T, u: U, v: V): Out
  }

  object combine extends Poly {
    implicit def onlyCase[Rec <: HList, K <: Symbol, V]
    (implicit vlv: FromLua[V],
              updater: Updater[Rec, FieldType[K, V]]) =
      use { (cur: Rec, field: (K, V), v: LuaValue) =>
        val fieldKey: K = field._1
        val luaValue = v.get(fieldKey.name)
        val newValue: V = vlv(luaValue)
        val newRecord = cur + (new FieldBuilder[K]()(newValue))
        newRecord
    }
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
