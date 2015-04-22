package com.github.puscala

import java.io.UnsupportedEncodingException

object Utilities {

  def toUTF8Bytes(s: String): Array[Byte] = try {
    s.getBytes("UTF-8")
  } catch {
    case e: UnsupportedEncodingException =>
      throw new Exception(e)
  }

}
