package com.github.puscala

import java.io.{ByteArrayOutputStream, DataOutputStream, IOException}
import java.{util => JUtil}

trait ApnsNotification {

  def getDeviceToken: Array[Byte]

  def getPayload: Array[Byte]

  def getIdentifier: Int

  def getExpiry: Int

}

sealed case class EnhancedNotificationFields(identifier: Int, expiry: Int, deviceToken: Array[Byte], payload: Array[Byte])

/**
 * APNS notification to be sent to Apple server
 * @param fields instance fields
 */
class EnhancedApnsNotification(fields: EnhancedNotificationFields) extends ApnsNotification {

  private val command: Byte = 1
  private val identifier = fields.identifier
  private val expiry = fields.expiry
  private val deviceTokenBytes = fields.deviceToken
  private val payloadBytes = fields.payload
  private lazy val marshall = marshallEnhanced(command, identifier, expiry, deviceTokenBytes, payloadBytes).clone()

  /**
   * constructs an instance of ApnsNotification
   * message encodes the payload with UTF-8 encoding
   * @param identifier notification id
   * @param expiry expire time
   * @param deviceToken device token to be sent
   * @param payload payload message to be sent
   * @return
   */
  def this(identifier: Int, expiry: Int, deviceToken: String, payload: String) =
    this(EnhancedNotificationFields(identifier, expiry, Utilities.decodeHex(deviceToken), Utilities.toUTF8Bytes(payload)))

  /**
   * constructs an instance of ApnsNotification
   * message encodes the payload with UTF-8 encoding
   * @param identifier notification id
   * @param expiry expire time
   * @param deviceTokenBytes binary representation of the device token
   * @param payloadBytes binary representation of the payload message
   * @return
   */
  def this(identifier: Int, expiry: Int, deviceTokenBytes: Array[Byte], payloadBytes: Array[Byte]) =
    this(EnhancedNotificationFields(identifier, expiry, deviceTokenBytes, payloadBytes))

  /**
   * Return the binary representation of the device token
   * @return
   */
  def getDeviceToken: Array[Byte] = Utilities.copyOf(deviceTokenBytes)

  /**
   * Return the binary representation of the payload message
   * @return
   */
  def getPayload: Array[Byte] = Utilities.copyOf(payloadBytes)

  /**
   * Return notification id
   * @return
   */
  def getIdentifier: Int = identifier

  /**
   * Return expiry time
   * @return
   */
  def getExpiry: Int = expiry

  /**
   * Return the length of the message in bytes as it is encoded on the wire
   * @return
   */
  def length: Int = {
    val l = 1 + 4 + 4 + 2 + deviceTokenBytes.length + 2 + payloadBytes.length
    val marshalledLength = marshall.length

    assert(marshalledLength == l)
    l
  }

  override def hashCode: Int =
    21 + 31 * identifier + 31 * expiry + 31 * JUtil.Arrays.hashCode(deviceTokenBytes) + 31 * JUtil.Arrays.hashCode(payloadBytes)

  override def equals(obj: Any): Boolean = obj match {
    case o: EnhancedApnsNotification =>
      identifier == o.identifier &&
      expiry == o.expiry &&
      JUtil.Arrays.equals(deviceTokenBytes, o.deviceTokenBytes) &&
      JUtil.Arrays.equals(payloadBytes, o.payloadBytes)
    case _ => false
  }

  override def toString: String = {
    val payloadString = try {
      new String(payloadBytes, "UTF-8")
    } catch {
      case _: Throwable => "???"
    }

    s"Message(id=$identifier, deviceToken=${Utilities.encodeHex(deviceTokenBytes)}, payload=$payloadString"
  }

  /**
   * Return the binary representation of the message as expected by the APNS server.
   * @param command notification command
   * @param identifier notification id
   * @param expiryTime expire time
   * @param tokenBytes binary representation of the device token
   * @param payloadBytes binary representation of the payload message
   * @return
   */
  private def marshallEnhanced(
    command: Byte,
    identifier: Int,
    expiryTime: Int,
    tokenBytes: Array[Byte],
    payloadBytes: Array[Byte]
  ): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val dos = new DataOutputStream(baos)

    try {
      dos.writeByte(command)
      dos.writeInt(identifier)
      dos.writeInt(expiry)
      dos.writeShort(tokenBytes.length)
      dos.write(tokenBytes)
      dos.writeShort(payloadBytes.length)
      dos.write(payloadBytes)

      baos.toByteArray
    } catch {
      case e: IOException => throw new AssertionError()
    } finally {
      dos.close()
      baos.close()
    }
  }

}
