package jsonnetcaseclass.types.complex

import org.scalatest.{FlatSpec, Matchers}

case class FooBar(fool: String)
case class HelloWorld(name: String, goodbye: FooBar)

class JsonnetClassSpec extends FlatSpec with Matchers {
  val expectedString =
    """
      |/*** CODE GENED - DO NOT EDIT ***/
      |
      |/*
      | * @param {String} name
      | * @param {FooBar} goodbye
      | * @return {HelloWorld}
      | */
      |local HelloWorld(name, goodbye) =
      |    {
      |        name : name,
      |        goodbye : goodbye,
      |
      |        /*
      |         * @return {String}
      |         */
      |        name :: $.name,
      |
      |        /*
      |         * @return {FooBar}
      |         */
      |        goodbye :: $.goodbye,
      |    };
      |{
      |  HelloWorld:: HelloWorld,
      |}
    """.stripMargin

  "A JsonnetClass" should "print out" in {
    JsonnetClass.fromType[HelloWorld].print shouldEqual(expectedString)
  }
}
