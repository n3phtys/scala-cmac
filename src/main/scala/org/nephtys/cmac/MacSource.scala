package org.nephtys.cmac

import javax.crypto.{KeyGenerator, Mac, SecretKey}

/**
  * Created by nephtys on 9/28/16.
  */
class MacSource(keySource : SecretKey) {

  lazy val mac : Mac = initializeMAC(keySource)

  private def initializeMAC(key : SecretKey) = {
    val mac = Mac.getInstance(key.getAlgorithm)
    mac.init(key)
    mac
  }



}
