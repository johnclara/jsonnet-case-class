/*
 * Copyright 2019 John Clara
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package jsonnetcaseclass.types

import jsonnetcaseclass.types.complex.JsonnetClass
import jsonnetcaseclass.types.primitives.{JsonnetBoolean, JsonnetNull, JsonnetNumber, JsonnetString}

import scala.reflect.runtime.universe._
import scala.util.Try

trait JsonnetType

object JsonnetType {

  object Cache {
    // primitive type cache table
    private[this] val primitiveTypeCache: Map[Type, JsonnetType] = Map(
      typeOf[Unit] -> JsonnetNull,
      typeOf[Boolean] -> JsonnetBoolean,
      typeOf[Double] -> JsonnetNumber,
      typeOf[Float] -> JsonnetNumber,
      typeOf[Byte] -> JsonnetNumber,
      typeOf[Char] -> JsonnetNumber,
      typeOf[Short] -> JsonnetNumber,
      typeOf[Int] -> JsonnetNumber,
      typeOf[Long] -> JsonnetNumber,
      typeOf[String] -> JsonnetString
    )

    // complex type cache table, initially empty
    private[this] var complexTypeCache = Map[Type, JsonnetType]()

    protected[jsonnetcaseclass] def resolve(tpe: Type): Option[JsonnetType] =
      primitiveTypeCache.get(tpe).orElse {
        complexTypeCache.get(tpe)
      }

    protected[jsonnetcaseclass] def save(tpe: Type, avroType: JsonnetType) {
      complexTypeCache = complexTypeCache + (tpe -> avroType)
    }

  }

  def apply[T: TypeTag]: JsonnetType = fromType[T].get

  def fromType[T : TypeTag]: Try[JsonnetType] = Try {
    val tt = typeTag[T]
    val tpe = tt.tpe
    Cache.resolve(tpe) match {
      case Some(jsonnetType) => jsonnetType
      case None =>
        val newComplexType = {
          // case classes
          if (tpe <:< typeOf[Product] && tpe.typeSymbol.asClass.isCaseClass)
            JsonnetClass.fromType(typeTag[T].asInstanceOf[TypeTag[Product]])
          /* TODO
          // sequences and sets
          else if (tpe <:< typeOf[Seq[_]] || tpe <:< typeOf[Array[_]] || tpe <:< typeOf[Set[_]])
            JsonnetArray.fromType(tt.asInstanceOf[TypeTag[_]])
          // string-keyed maps
          else if (tpe <:< typeOf[Map[String, _]])
            JsonnetMap.fromType(tt.asInstanceOf[TypeTag[_]])
          */
          else
            throw new IllegalArgumentException(s"Unhandled type: ${tpe}")
        }

        Cache.save(tpe, newComplexType)

        newComplexType
    }
  }
}
