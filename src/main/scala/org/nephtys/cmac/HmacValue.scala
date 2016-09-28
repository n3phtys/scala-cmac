package org.nephtys.cmac

/**
  *
  * uses upickle, so serialization limits of upickle apply here too
  * Created by nephtys on 9/28/16.
  */
case class HmacValue[T](t : T, hmac : String) {

  case class HmacValueStringed(t : String, hmac : String)


  import upickle.default._
  import HmacHelper._

  def checkHMAC()(implicit macSource: MacSource,
  writer : upickle.default.Writer[T]
  ) : Boolean = {
    implicit val mac = macSource.mac
    HmacHelper.checkHMAC[T](t, hmac)
  }

  def toURLEncodedString()(implicit writer : upickle.default.Writer[T]
  ) : URLEncodedString =  jsonCompact().toURLEncodedString

  def jsonCompact()(implicit writer : upickle.default.Writer[T]
  ) : String = {
    val inner : String = write(this)
    //val inner : String = writer.write(this.t)
    //write(HmacValueStringed(inner, hmac))
    inner
  }
  def jsonPrettyPrint()(implicit writer : upickle.default.Writer[T]
  ) : String = ???
}


