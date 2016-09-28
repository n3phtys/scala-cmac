package org.nephtys.cmac

/**
  * Created by nephtys on 9/28/16.
  */
case class URLEncodedString(encodedString : String) {
  import org.nephtys.cmac.HmacHelper._
  import upickle.default._
  def toNotURLEncoded : NotURLEncodedString = encodedString.fromURLEncodedString

  def toHMAC[T]()(implicit reader : upickle.default.Reader[T]) = {
    toNotURLEncoded.toHMAC[T]()
  }
}
