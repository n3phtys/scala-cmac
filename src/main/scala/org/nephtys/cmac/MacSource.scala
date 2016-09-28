package org.nephtys.cmac

import javax.crypto.{KeyGenerator, Mac, SecretKey}

/**
  * Created by nephtys on 9/28/16.
  */
class MacSource(keySource : () => SecretKey) {

  private var lastKey : Option[SecretKey] = None
  private var lastMac : Option[Mac] = None

  def mac : Mac = this.synchronized {
    val newKey = keySource()
    if (lastMac.isDefined && lastKey.forall(k => k.equals(newKey))) {
      lastMac.get
    } else {
        val m = initializeMAC(newKey)
      lastMac = Some(m)
      m
    }

  }


  private def initializeMAC(key : SecretKey) = {
    val mac = Mac.getInstance(key.getAlgorithm)
    mac.init(key)
    mac
  }



}
