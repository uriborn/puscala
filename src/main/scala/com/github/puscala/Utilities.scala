package com.github.puscala

import java.io.UnsupportedEncodingException
import java.util.regex.Pattern
import scala.collection.mutable

object Utilities {

  def toUTF8Bytes(s: String): Array[Byte] = try {
    s.getBytes("UTF-8")
  } catch {
    case e: UnsupportedEncodingException =>
      throw new Exception(e)
  }

  def copyOf(bytes: Array[Byte]): Array[Byte] = {
    val copy = new Array[Byte](bytes.length)
    System.arraycopy(bytes, 0, copy, 0, bytes.length)

    copy
  }

  def encodeHex(bytes: Array[Byte]): String = {
    val base = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
    val chars = mutable.ArrayBuffer[Char]()

    (0 until bytes.length).foreach { i =>
      val b = bytes(i) & 0xFF
      chars += base(b >>> 4)
      chars += base(b & 0xF)
    }

    new String(chars.toArray)
  }

  def decodeHex(deviceToken: String): Array[Byte] = {
    val pattern = Pattern.compile("[ -]")
    val hex = pattern.matcher(deviceToken).replaceAll("")
    val bytes = mutable.ArrayBuffer[Byte]()

    (0 until (hex.length / 2)).foreach { i =>
      bytes += (charToHex(hex.charAt(2 * i)) * 16 + charToHex(hex.charAt(2 * i + 1))).toByte
    }

    bytes.toArray
  }

  private def charToHex(char: Char): Int = {
    if ('0' <= char && char <= '9') char - '0'
    else if ('a' <= char && char <= 'f') (char - 'a') + 10
    else if ('A' <= char && char <= 'F') (char - 'A') + 10
    else throw new RuntimeException(s"Invalid hex character: $char")
  }

}
