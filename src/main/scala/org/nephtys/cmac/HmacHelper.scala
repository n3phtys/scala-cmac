package org.nephtys.cmac

import java.net.{URLDecoder, URLEncoder}
import java.nio.charset.StandardCharsets
import java.util.Base64
import javax.crypto.spec.SecretKeySpec
import javax.crypto.{KeyGenerator, Mac, SecretKey}

/**
  * Created by nephtys on 9/28/16.
  */
object HmacHelper {
  //implicit converter to HmacValue from Any, taking in MacGetter

  implicit class AnyToHmac[T](x: T) {
    def toHMAC()(implicit macSource: MacSource, writer : upickle.default.Writer[T]): HmacValue[T] = {
      implicit val mac = macSource.mac
      HmacValue[T](x, computeHMAC[T](x))
    }
  }

  implicit class StringURLEncodingExtension(str: String) {
    def fromURLEncodedString: NotURLEncodedString = {
      NotURLEncodedString(urlUnencode(str))
    }
    def toURLEncodedString: URLEncodedString = {
      URLEncodedString(urlEncode(str))
    }
  }

  import upickle.default._

  def checkHMAC[T](t : T, hmac : String)(implicit mac : Mac,
                                         writer : upickle.default.Writer[T]) : Boolean = {
    val calculatedMac : String = computeHMAC(t)
    calculatedMac == hmac
  }
  def computeHMAC[T](t : T)(implicit mac : Mac, writer : upickle.default.Writer[T]) : String = {
    val msg : String = write[T](t)
    Base64.getEncoder.encodeToString(mac.doFinal(msg.getBytes("UTF-8")))
  }


  def urlEncode(str : String) = Base64.getUrlEncoder.encodeToString(str.getBytes(StandardCharsets.UTF_8)).replace('=', '.')
  def urlUnencode(encodedStr : String) = new String(Base64.getUrlDecoder.decode(encodedStr.replace('.', '=')),
  StandardCharsets
    .UTF_8)



  object keys {
    //helper methods for secretkey taken from http://stackoverflow.com/a/12039611/1907778

    def readFromBase64String(base64str : String, algorithm : String = "AES") : SecretKey = {
      // decode the base64 encoded string
      val decodedKey = Base64.getDecoder.decode(base64str)
      // rebuild key using SecretKeySpec
      val originalKey = new SecretKeySpec(decodedKey, 0, decodedKey.length, algorithm)
      originalKey
    }
    def writeToBase64String(secretKey : SecretKey, algorithm : String = "AES") : String = {
      // get base64 encoded version of the key
      val encodedKey = Base64.getEncoder.encodeToString(secretKey.getEncoded)
      encodedKey
    }

    def generateNewKey(bits : Int, algorithm : String = "AES") : SecretKey = {
      val keyGen = KeyGenerator.getInstance(algorithm)
      keyGen.init(bits)
      val key = keyGen.generateKey()
      key
    }
  }
}
