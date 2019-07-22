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

package jsonnetcaseclass.scalavro.util

import scala.collection.immutable.ListMap
import scala.reflect.api.{TypeCreator, Universe, Mirror}
import scala.reflect.runtime.universe._

/**
  * Companion object for [[ReflectionHelpers]]
  */
object ReflectionHelpers extends ReflectionHelpers

trait ReflectionHelpers {

  val classLoaderMirror = runtimeMirror(getClass.getClassLoader)

  /**
    * Returns a map from formal parameter names to type tags, containing one
    * mapping for each constructor argument.  The resulting map (a ListMap)
    * preserves the order of the primary constructor's parameter list.
    *
    * @tparam T  the type of the case class to inspect
    */
  def caseClassParamsOf[T: TypeTag]: ListMap[String, TypeTag[_]] = {
    val tpe = typeOf[T]
    val constructorSymbol = tpe.decl(termNames.CONSTRUCTOR)
    val defaultConstructor =
      if (constructorSymbol.isMethod) constructorSymbol.asMethod
      else {
        val ctors = constructorSymbol.asTerm.alternatives
        ctors.map { _.asMethod }.find { _.isPrimaryConstructor }.get
      }

    ListMap.empty[String, TypeTag[_]] ++ defaultConstructor.paramLists.reduceLeft(_ ++ _).map {
      sym => sym.name.toString -> tagForType(tpe.member(sym.name).asMethod.returnType)
    }
  }

  /**
    * Returns a TypeTag in the current runtime universe for the supplied type.
    */
  def tagForType(tpe: Type): TypeTag[_] = TypeTag(
    classLoaderMirror,
    new TypeCreator {
      def apply[U <: Universe with Singleton](m: Mirror[U]) = tpe.asInstanceOf[U#Type]
    }
  )


  /**
    * Returns `Some(value)` if there is a default value for the supplied
    * parameter name for the supplied case class type and `None` otherwise.
    * If the supplied parameter is not defined for the type's apply method,
    * this method simply returns `None`.
    *
    * @tparam T         the type of the case class to inspect
    * @param parameter  the name of the parameter to find a default value for
    */
  def defaultCaseClassValues[T: TypeTag]: Map[String, Option[Any]] = {
    val companion = CompanionMetadata[T].get

    val applySymbol: MethodSymbol = {
      val symbol = companion.classType.member(TermName("apply"))
      if (symbol.isMethod) symbol.asMethod
      else symbol.asTerm.alternatives.head.asMethod // symbol.isTerm
    }

    def valueFor(i: Int): Option[Any] = {
      val defaultValueThunkName = TermName(s"apply$$default$$${i + 1}")
      val defaultValueThunkSymbol = companion.classType member defaultValueThunkName

      if (defaultValueThunkSymbol == NoSymbol) None
      else {
        val defaultValueThunk = companion.instanceMirror reflectMethod defaultValueThunkSymbol.asMethod
        Some(defaultValueThunk.apply())
      }
    }

    applySymbol.paramLists.flatten.zipWithIndex.map { case (p, i) => p.name.toString -> valueFor(i) }.toMap
  }

  /**
    * Wraps information about a companion object for a type.
    */
  case class CompanionMetadata[T](
    symbol: ModuleSymbol,
    instance: Any,
    instanceMirror: InstanceMirror,
    classType: Type)

  object CompanionMetadata {
    /**
      * Returns a Some wrapping CompanionMetadata for the supplied class type, if
      * that class type has a companion, and None otherwise.
      */
    def apply[T: TypeTag]: Option[CompanionMetadata[T]] = {

      val typeSymbol = typeOf[T].typeSymbol

      val companion: Option[ModuleSymbol] = {
        if (!typeSymbol.isClass) None // supplied type is not a class
        else {
          val classSymbol = typeSymbol.asClass
          if (!classSymbol.companion.isModule) None // supplied class type has no companion
          else Some(classSymbol.companion.asModule)
        }
      }

      companion.map { symbol =>
        val instance = classLoaderMirror.reflectModule(symbol).instance
        val instanceMirror = classLoaderMirror.reflect(instance)
        val classType = symbol.moduleClass.asClass.asType.toType
        CompanionMetadata(symbol, instance, instanceMirror, classType)
      }
    }
  }
}
