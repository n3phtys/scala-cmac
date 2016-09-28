package org.nephtys.cmac

import java.net.{URLDecoder, URLEncoder}
import java.util.Base64
import javax.crypto.Mac

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


  def urlEncode(str : String) = URLEncoder.encode(str, "UTF-8")
  def urlUnencode(encodedStr : String) = URLDecoder.decode(encodedStr, "UTF-8")
}
