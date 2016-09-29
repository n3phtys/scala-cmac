package org.nephtys.cmac

import javax.crypto.SecretKey

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by nephtys on 9/29/16.
  */
class KeySpec extends FlatSpec with Matchers{
  val algorithm = "AES"
  val bits = 256

  "A Key" should "be created, written to a string and reread without problems" in {
    val generatedKey : SecretKey = HmacHelper.keys.generateNewKey(bits, algorithm)

    val stringifiedKey : String = HmacHelper.keys.writeToBase64String(generatedKey, algorithm)

    val regeneratedKey : SecretKey = HmacHelper.keys.readFromBase64String(stringifiedKey, algorithm)

    generatedKey.equals(regeneratedKey) should be (true)
  }
}
