package org.nephtys.cmac

import java.net.{URLDecoder, URLEncoder}
import java.nio.charset.StandardCharsets
import javax.crypto.spec.SecretKeySpec
import javax.crypto.{KeyGenerator, Mac, SecretKey}
import javax.xml.bind.DatatypeConverter

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
    DatatypeConverter.printBase64Binary(mac.doFinal(msg.getBytes("UTF-8")))
  }


  /**
    * uses UTF-8 and - _ as additional symbols
    * @param str
    * @return
    */
  def urlB64encode(str : String) : String = DatatypeConverter.printBase64Binary(str.getBytes("UTF-8")).replace('+',
    '-').replace('/', '_').replace('=', '.')
  def urlB64decode(str : String) : String = new String(DatatypeConverter.parseBase64Binary(str.replace('-',
    '+').replace('_', '/').replace('.', '=')), "UTF-8")

  def urlEncode(str : String) = urlB64encode(str)
  def urlUnencode(encodedStr : String) = urlB64decode(encodedStr)

  object keys {
    //helper methods for secretkey taken from http://stackoverflow.com/a/12039611/1907778

    def readFromBase64String(base64str : String, algorithm : String) : SecretKey = {
      // decode the base64 encoded string
      println(s"read from base64str : $base64str")

      //the next is a hack to prevent LF from hanging around
      def lastchar : Char = base64str.toCharArray.last
      val decodedKey = DatatypeConverter.parseBase64Binary(if (lastchar == 10) base64str.take(base64str.size - 1) else base64str)
      // rebuild key using SecretKeySpec
      val originalKey = new SecretKeySpec(decodedKey, 0, decodedKey.length, algorithm)
      originalKey
    }
    def writeToBase64String(secretKey : SecretKey) : String = {

      println(s"write to base64str: ${Base64.getEncoder.encodeToString(secretKey.getEncoded)}")
      // get base64 encoded version of the key
      val encodedKey = DatatypeConverter.printBase64Binary(secretKey.getEncoded)
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
