package org.ucombinator.webapp.util

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Base64.encodeBase64

object Password {
  val keyStr = "TapasKey"
  val key = new SecretKeySpec(keyStr.getBytes, "HmacSHA1")
  val mac = Mac.getInstance("HmacSHA1")
  mac.init(key)

  def apply(password: String): String = {
    val hashed = mac.doFinal(password.getBytes)
    mac.reset
    new String(encodeBase64(hashed))
  }
}
