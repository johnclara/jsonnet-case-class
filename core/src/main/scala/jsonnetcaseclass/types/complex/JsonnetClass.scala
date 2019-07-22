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

package jsonnetcaseclass.types.complex

import jsonnetcaseclass.scalavro.util.ReflectionHelpers
import jsonnetcaseclass.types.JsonnetType

import scala.reflect.runtime.universe._

case class JsonnetClass(name: String, fields: Seq[JsonnetClass.Field[_]]) extends JsonnetType {
  def print: String =
    s"""
      |/*** CODE GENED - DO NOT EDIT ***/
      |
      |/*
      |${fields.map(_.printParamNote(" ")).mkString("\n|")}
      | * @return {$name}
      | */
      |local $name(${fields.map(_.printParam).mkString(", ")}) =
      |    {
      |${fields.map(_.printVar(" " * 8)).mkString("\n|")}
      |${fields.map(_.printGetter(" " * 8)).mkString("\n|")}
      |    };
      |{
      |  $name:: $name,
      |}
    """.stripMargin
}

object JsonnetClass {
  def fromType[T <: Product: TypeTag] = {
    val tt = typeTag[T]
    val classSymbol = tt.tpe.typeSymbol.asClass
    if (classSymbol.isCaseClass && classSymbol.typeParams.isEmpty) {
      tt.tpe match {
        case TypeRef(prefix, symbol, _) =>

          val defaultValues = ReflectionHelpers.defaultCaseClassValues[T]

          new JsonnetClass(
            name = symbol.name.toString,
            fields = ReflectionHelpers.caseClassParamsOf[T].toSeq map {
              case (name, tag) => JsonnetClass.Field(name, defaultValues(name))(tag.asInstanceOf[TypeTag[Any]])
            }
          )
      }
    }
    else throw new IllegalArgumentException(s"""
      |Could not create an JsonnetClass from type [${tt.tpe}]
      |Product types must be case classes with no type parameters
    """.stripMargin)
  }

   case class Field[T: TypeTag](name: String, default: Option[T]) {

     lazy val fieldType: JsonnetType = JsonnetType[T]

     def printParamNote(padding: String): String = s"$padding* @param {${typeTag.tpe.typeSymbol.name}} $name"

     def printParam: String = default match {
       case Some(value) => s"$name = $value"
       case None => s"$name"
     }
     def printVar(padding: String): String = s"$padding$name : $name,"

     def printGetter(padding: String): String =
       s"""
          |$padding/*
          |$padding * @return {${typeTag.tpe.typeSymbol.name}}
          |$padding */
          |$padding$name :: $$.$name,""".stripMargin
   }
}
