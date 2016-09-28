package org.nephtys.cmac

import javax.crypto.KeyGenerator

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by nephtys on 9/28/16.
  */
class CmacSpec extends FlatSpec with Matchers {
  import org.nephtys.cmac.HmacHelper._
  import upickle.default._

  private val bits = 4096
  private val keyGen = KeyGenerator.getInstance("HmacMD5")
  private val generatedKey = keyGen.generateKey()

  implicit val macSource = new MacSource(() => generatedKey)

  case class TestClass(value : Int, name : String)

  val tv1 = TestClass(42, "This is #42")
  val tv2 = TestClass(13, "This is #13")

  val teststring : String  = "This is a very complicated string you know! {} has ? $ dsg _. #'"


  "A String" should "be convertable to URLEncoding and back without loss" in {
    val converted : String = teststring.toURLEncodedString.encodedString

    val backconverted : String = converted.fromURLEncodedString.unencodedString

    backconverted should be (teststring)
  }

  "A Value" should "be convertable to a HmacValue and check true" in {
    val hmacvalue = tv1.toHMAC()
    hmacvalue.checkHMAC() should be (true)
  }

  it should "not check true if it was interchanged" in {
    val hmacvalue = tv1.toHMAC()
    val changedhmacvalue = hmacvalue.copy(t = tv2)
    changedhmacvalue.checkHMAC() should be (false)
  }

  it should "be convertable to hmac, moved into a string, converted back, checked, and the value be returned" in {
    val hmac = tv1.toHMAC()
    val urlencoded = hmac.toURLEncodedString
    val unencoded = urlencoded.toNotURLEncoded
    val hmacRead = read[HmacValue[TestClass]](unencoded.unencodedString)
    hmacRead.checkHMAC() should be (true)
    hmacRead.t should be (tv1)
    hmacRead.t should not be (tv2)
  }
}
