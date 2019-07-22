package jsonnetcaseclass.types

import jsonnetcaseclass.types.complex.JsonnetClass
import jsonnetcaseclass.types.primitives.JsonnetString
import org.scalatest.{FlatSpec, Matchers}

case class HelloWorld(name: String)

case class RecursiveClass(inner: RecursiveClass)

class JsonnetTypeSpec extends FlatSpec with Matchers {

  "A JsonnetType" should "convert from strings" in {
    JsonnetType[String].shouldEqual(JsonnetString)
  }

  it should "convert from case classes" in {
    val jsonnetClass = JsonnetType[HelloWorld].asInstanceOf[JsonnetClass]
    jsonnetClass.name.shouldEqual("HelloWorld")
    jsonnetClass.fields.head.fieldType.shouldEqual(JsonnetString)
  }

  it should "handle recursive case classes" in {
    val jsonnetClass = JsonnetType[RecursiveClass].asInstanceOf[JsonnetClass]

    jsonnetClass.name.shouldEqual("RecursiveClass")

    jsonnetClass.fields.head.fieldType.shouldEqual(jsonnetClass)
  }
}
