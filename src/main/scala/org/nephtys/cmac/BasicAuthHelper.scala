package org.nephtys.cmac

import java.nio.charset.StandardCharsets
import java.util.Base64

/**
  * always asssumes utf8 strings
  * Created by nephtys on 9/30/16.
  */
object BasicAuthHelper {
    case class LoginData(username : String, password : String)


    val splitCharacterBetweenUsernameAndPassword = ':'

    def extractPasswordFromAuthenticationHeaderValue(headervalue : String) : Option[LoginData] = {
      val combined = decodeB64(extractB64PartFromString(headervalue))
      val splitarr : Array[String] = combined.split(splitCharacterBetweenUsernameAndPassword)
      val splitfilteredarr : Array[String] = splitarr.filter(_.length > 0)
      if (splitfilteredarr.length == 2)  {
        val username : String = splitfilteredarr(0)
        val password : String = splitfilteredarr(1)
        Some(LoginData(username, password))
      } else {
        None
      }
    }

    def extractB64PartFromString(rawstring : String) : String = {
      val split : Array[String] = rawstring.split(" +") //regular expresion
      val filter : Array[String] = split.filter(_.length > 2) //sanity check
      filter.lastOption.getOrElse("")
    }

    def decodeB64(base64str : String) : String = {
      val bytes =  Base64.getDecoder.decode(base64str)
      val str = new String(bytes, StandardCharsets.UTF_8)
      str
    }
}
