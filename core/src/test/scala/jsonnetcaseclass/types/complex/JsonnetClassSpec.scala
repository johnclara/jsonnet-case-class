package jsonnetcaseclass.types.complex

import jsonnetcaseclass.io.{Format, LongTypeHint, ShortTypeHint}
import org.scalatest.{FlatSpec, Matchers}

case class FooBar(fool: String)
case class HelloWorld(name: String, goodbye: FooBar)

class JsonnetClassSpec extends FlatSpec with Matchers {

  "A JsonnetClass" should "print out" in {
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
        |    HelloWorld :: HelloWorld,
        |}""".stripMargin
    JsonnetClass.fromType[HelloWorld].print(Format(4, None)).shouldEqual(expectedString)
  }

  it should "add short type hints" in {
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
        |        jsonClass : HelloWorld,
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
        |    HelloWorld :: HelloWorld,
        |}""".stripMargin
    JsonnetClass.fromType[HelloWorld].print(Format(4, Some(ShortTypeHint("jsonClass")))).shouldEqual(expectedString)
  }

  it should "add long type hints" in {
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
        |        jsonClass : jsonnetcaseclass.types.complex.HelloWorld,
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
        |    HelloWorld :: HelloWorld,
        |}""".stripMargin
    JsonnetClass.fromType[HelloWorld].print(Format(4, Some(LongTypeHint("jsonClass")))).shouldEqual(expectedString)
  }
}
