package org.nephtys.cmac

import java.nio.charset.StandardCharsets
import java.util.Base64
import javax.crypto.SecretKey

import org.nephtys.cmac.BasicAuthHelper.LoginData
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by nephtys on 9/30/16.
  */
class BasicAuthSpec  extends FlatSpec with Matchers{

  val password = "OpenSesame"
  val username = "Aladdin"
  val combinedString : String = username+""":"""+password
  val b64string : String = """QWxhZGRpbjpPcGVuU2VzYW1l"""
  val headervalue : String = """Basic QWxhZGRpbjpPcGVuU2VzYW1l"""

  "Extracting B64 Part from String" should "work normally in simple case" in {
    BasicAuthHelper.extractB64PartFromString(headervalue) should be (b64string)
  }
  "Extracting B64 Part from String" should "work normally with additional whitespaces" in {
    val newvalue = """       """+headervalue+"""           """
    BasicAuthHelper.extractB64PartFromString(newvalue) should be (b64string)
  }
  "Extracting B64 Part from String" should "work normally with more than two parts" in {
    val newvalue = """  Some kind of     """+b64string+"""           """
    BasicAuthHelper.extractB64PartFromString(newvalue) should be (b64string)
  }
  "Extracting B64 Part from String" should "work normally with only one part" in {
    val newvalue = """       """+b64string+"""           """
    BasicAuthHelper.extractB64PartFromString(newvalue) should be (b64string)
  }

  "The B64 encoding" should "work as expected in simple cases" in {
    val str = Base64.getEncoder.encodeToString("user:pass".getBytes(StandardCharsets.UTF_8))
    str should be ("""dXNlcjpwYXNz""")
  }

  "The B64 encoding" should "work as expected in normal cases" in {
    val str = Base64.getEncoder.encodeToString(combinedString.getBytes(StandardCharsets.UTF_8))
    str should be (b64string)
  }

  "The B64 decoding" should "result in the original value" in {
    BasicAuthHelper.decodeB64(b64string) should be (combinedString)
  }

  "A Basic Auth Authorization" should "be converted back to the correct values" in {
    BasicAuthHelper.extractPasswordFromAuthenticationHeaderValue(headervalue) should be (Some(LoginData(username,
      password)))
  }
}
