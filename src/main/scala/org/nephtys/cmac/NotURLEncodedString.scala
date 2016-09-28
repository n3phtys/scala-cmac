package org.nephtys.cmac

/**
  * Created by nephtys on 9/28/16.
  */
case class NotURLEncodedString(unencodedString : String) {
  import org.nephtys.cmac.HmacHelper._
  import upickle.default._
  def toURLEncoded : URLEncodedString = unencodedString.toURLEncodedString

  def toHMAC[T]()(implicit reader : upickle.default.Reader[T]) : HmacValue[T] = {
    read[HmacValue[T]](unencodedString)
  }
}
