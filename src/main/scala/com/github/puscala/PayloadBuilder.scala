package com.github.puscala

import scala.annotation.tailrec

import net.liftweb.json._
  import Extraction._

sealed trait HasBody
sealed trait SetBody extends HasBody
sealed trait NotSetBody extends HasBody

private[puscala] case class Payload[HasBody](root: Map[String, Any], aps: Map[String, Any], custom: Map[String, Any])

sealed abstract class PayloadBuilderBase[T](payload: Payload[HasBody]) {

  type PayloadWithBody = Payload[SetBody]
  type PayloadWithoutBody = Payload[NotSetBody]

  type ThisPayload = Payload[T]

  implicit val formats = DefaultFormats

  val maxPayloadLength = 2048

  /**
   * Return Json String of the constructed payload
   * @return
   */
  def build: String = {
    val buildPayload = {
      if (!payload.root.contains("mdm")) {
        val apsField: Map[String, Any] = payload.custom.size match {
          case 0 => payload.aps - "alert"
          case 1 if payload.custom.contains("body") => payload.aps + ("alert" -> payload.custom("body"))
          case _ => payload.aps + ("alert" -> payload.custom)
        }

        payload.root + ("aps" -> apsField)
      } else
        payload.root
    }

    compact(render(decompose(buildPayload)))
  }

  def length: Int

  /**
   * Return true if the payload built so far is larger than size permitted by Apple
   * (which is 256 or 2048 bytes)
   * @return
   */
  def isTooLong: Boolean = length > maxPayloadLength

  /**
   * Return bytes of the payload
   * @return
   */
  def buildBytes: Array[Byte] = Utilities.toUTF8Bytes(build)

  def truncateWhenUTF8(s: String, maxByte: Int): String = {

    @tailrec
    def recursive(byte: Int, idx: Int): String = {
      val char = s.charAt(idx)
      val (skip, more) = char match {
        case c if c <= 0x007f => (0, 1)
        case c if c <= 0x07FF => (0, 2)
        case c if c <= 0xd7ff => (0, 3)
        case c if c <= 0xDFFF => (1, 4)
        case _                => (0, 3)
      }

      if (byte + more > maxByte) s.substring(0, idx) else recursive(byte + more, idx + 1 + skip)
    }

    recursive(0, 0)
  }

  /**
   * Set to the root field
   * @param field field name and value pair
   * @tparam A
   * @return
   */
  def putRoot[A <: HasBody](field: (String, Any)): Payload[A] = payload.copy(root = payload.root + field)

  /**
   * Set to the aps field
   * @param field field name and value pair
   * @tparam A
   * @return
   */
  def putAps[A <: HasBody](field: (String, Any)): Payload[A] = payload.copy(aps = payload.aps + field)

  /**
   * Set to the custom field
   * @param field field name and value pair
   * @tparam A
   * @return
   */
  def putCustom[A <: HasBody](field: (String, Any)): Payload[A] = payload.copy(custom = payload.custom + field)

}

object PayloadBuilder {

  def apply(): PayloadBuilder[NotSetBody] = new PayloadBuilder[NotSetBody]()

}

private[puscala] class PayloadBuilder[T <: HasBody](
  payload: Payload[HasBody] = Payload(Map[String, Any](), Map[String, Any](), Map[String, Any]())
) extends PayloadBuilderBase[T](payload) with Logging {

  type This = PayloadBuilder[T]

  /**
   * Set alert body
   * @param body text to appear
   * @return
   */
  def body(body: String): PayloadBuilder[SetBody] =
    new PayloadBuilder[SetBody](putCustom("body" -> body))

  /**
   * Set sound to be played
   * @param sound file name or song name to be played
   * @return
   */
  def sound(sound: String): This =
    new PayloadBuilder(putAps("sound" -> sound))

  /**
   * Set category of the notification for iOS8 notification actions
   * @param category name of the category supplied to the app
   * @return
   */
  def category(category: String): This =
    new PayloadBuilder(putAps("category" -> category))

  /**
   * Set the notification badge to be displayed
   * @param badge badge number to be displayed
   * @return
   */
  def badge(badge: Int): This =
    new PayloadBuilder(putAps("badge" -> badge))

  /**
   * Cleaning of budge number
   * @return
   */
  def clearBadge: This = badge(0)

  /**
   * Set value of action button (the right button to be displayed).
   * Default value is "View"
   * The value can be either simple String or a localizable key.
   * None is that actionKey indicates no additional button.
   * @param actionKey title or localizable key
   * @return
   */
  def actionKey(actionKey: Option[String]): This = {
    val key = if (actionKey.isDefined) actionKey else null

    new PayloadBuilder(putCustom("action-loc-key" -> key))
  }

  /**
   * Set the notification view to display an action button
   * @return
   */
  def noActionButton: This = actionKey(None)

  /**
   * Set notification type of be a "newsstand" notification.
   * @return
   */
  def forNewsstand: This =
    new PayloadBuilder(putAps("content-available" -> 1))

  /**
   * With iOS7 it is possible to have the application wake up before the user opens the app.
   * The same keyword can also be used to send "silent" notifications.
   * With these "silent" notification different app delegate is being invoked, allowing app to perform background tasks.
   * @return
   */
  def forBackground: This =
    new PayloadBuilder(putAps("content-available" -> 1))

  /**
   * Set notification localized key for body
   * @param key localizable key
   * @return
   */
  def localizedKey(key: String): This =
    new PayloadBuilder(putCustom("loc-key" -> key))

  /**
   * Set arguments for the alert message localizable message.
   * iPhone does not localize the arguments
   * @param args arguments to the localized alert message
   * @return
   */
  def localizedArgs(args: String*): This =
    new PayloadBuilder(putCustom("loc-args" -> args.toList))

  /**
   * Set launch image file
   * @param launchImage name of the image file in the application bundle.
   * @return
   */
  def launchImage(launchImage: String): This =
    new PayloadBuilder(putCustom("launch-image" -> launchImage))

  /**
   * Set any application-specific custom field.
   * The values are presented to the application and the iPhone does not display them automatically.
   * @param key custom field name
   * @param value custom field value
   * @return
   */
  def customField(key: String, value: Any): This = {
    if (key == "body")
      warn("If you set body fields using this method, you'll not be able to use #resizeBody, #shrinkBody.Use PayloadBuilder#body.")

    new PayloadBuilder(putRoot(key -> value))
  }

  /**
   * Set any application-specific custom field.
   * The values are presented to the application and the iPhone does not display them automatically.
   * @param fields custom field and value pair
   * @return
   */
  def customFields(fields: (String, Any)*): This = {
    val fieldMap = fields.toMap

    if (fieldMap.isDefinedAt("body"))
      warn("If you set body fields using this method, you'll not be able to use #resizeBody, #shrinkBody.Use PayloadBuilder#body.")

    new PayloadBuilder(payload.copy(root = payload.root ++ fieldMap))
  }

  /**
   * Set notification type of be a "newsstand" notification.
   * @param mdm value of operation
   * @return
   */
  def mdm(mdm: String): This = customField("mdm", mdm)

  /**
   * Shrinks body so that the resulting payload message fits within
   * passed expected payload length
   * @param payloadLength expected max size of payload
   * @param setBody
   * @return
   */
  def resizeBody(payloadLength: Int)(implicit setBody: ThisPayload =:= PayloadWithBody): PayloadBuilder[SetBody] =
    resizeBody(payloadLength, "")

  /**
   * Shrinks body so that the resulting payload message fits within
   * passed expected payload length
   * @param payloadLength expected max size of payload
   * @param postfix for the truncated body, e.g. "..."
   * @param setBody
   * @return
   */
  def resizeBody(payloadLength: Int, postfix: String)(implicit setBody: ThisPayload =:= PayloadWithBody): PayloadBuilder[SetBody] = {
    val currentLength = length
    if (currentLength <= payloadLength)
      copy
    else {
      val body = payload.custom("body").toString
      val acceptableSize =
        Utilities.toUTF8Bytes(body).length - (currentLength - payloadLength + Utilities.toUTF8Bytes(postfix).length)

      val resizedBody = truncateWhenUTF8(body, acceptableSize) + postfix

      new PayloadBuilder[SetBody](
        payload.copy(custom = payload.custom.updated("body", resizedBody))
      )
    }
  }

  /**
   * Shrinks body so that the resulting payload message fits within require Apple specification (2048 bytes)
   * @param setBody
   * @return
   */
  def shrinkBody(implicit setBody: ThisPayload =:= PayloadWithBody): PayloadBuilder[SetBody] = shrinkBody("")

  /**
   * Shrinks body so that the resulting payload message fits within require Apple specification (2048 bytes)
   * @param postfix for the truncated body, e.g. "..."
   * @param setBody
   * @return
   */
  def shrinkBody(postfix: String)(implicit setBody: ThisPayload =:= PayloadWithBody): PayloadBuilder[SetBody] =
    resizeBody(maxPayloadLength, postfix)

  /**
   * Return length of payload bytes once marshaled to bytes
   * @return
   */
  def length: Int = copy.buildBytes.length

  /**
   * Return copy of this builder
   * @tparam A
   * @return
   */
  def copy[A <: HasBody]: PayloadBuilder[A] = new PayloadBuilder[A](payload.copy())

  override def toString: String = build

}

object SafariPayloadBuilder {

  def apply(): SafariPayloadBuilder[NotSetBody] = new SafariPayloadBuilder[NotSetBody]()

}

private[puscala] class SafariPayloadBuilder[T <: HasBody](
  payload: Payload[HasBody] = Payload(Map[String, Any](), Map[String, Any](), Map[String, Any]())
) extends PayloadBuilderBase[T](payload) with Logging {

  type This = SafariPayloadBuilder[T]

  override val maxPayloadLength = 256

  /**
   * Set the alert body
   * @param body text to appear
   * @return
   */
  def body(body: String): SafariPayloadBuilder[SetBody] =
    new SafariPayloadBuilder[SetBody](putCustom("body" -> body))

  /**
   * Set the title for Safari Notifications
   * @param title text to appear
   * @return
   */
  def title(title: String): This =
    new SafariPayloadBuilder(putCustom("title" -> title))

  /**
   * Set the alert action for Safari Notifications
   * @param action label of action button
   * @return
   */
  def action(action: String): This =
    new SafariPayloadBuilder(putCustom("action" -> action))

  /**
   * Set the url-args key that are paired with the placeholders inside
   * the urlFormatString value of your website.json file.
   * Order of the placeholders in URL format string determines
   * order of the values supplied by the url-args.
   * @param urlArgs values to be paired with the placeholders
   * @return
   */
  def urlArgs(urlArgs: String*): This =
    new SafariPayloadBuilder(putAps("url-args" -> urlArgs.toList))

  /**
   * Shrinks body so that the resulting payload message fits within
   * passed expected payload length
   * @param payloadLength expected max size of payload
   * @param setBody
   * @return
   */
  def resizeBody(payloadLength: Int)(implicit setBody: ThisPayload =:= PayloadWithBody): SafariPayloadBuilder[SetBody] =
    resizeBody(payloadLength, "")

  /**
   * Shrinks body so that the resulting payload message fits within
   * passed expected payload length
   * @param payloadLength expected max size of payload
   * @param postfix for the truncated body, e.g. "..."
   * @param setBody
   * @return
   */
  def resizeBody(payloadLength: Int, postfix: String)(implicit setBody: ThisPayload =:= PayloadWithBody): SafariPayloadBuilder[SetBody] = {
    val currentLength = length
    if (currentLength <= payloadLength)
      copy
    else {
      val body = payload.custom("body").toString
      val acceptableSize =
        Utilities.toUTF8Bytes(body).length - (currentLength - payloadLength + Utilities.toUTF8Bytes(postfix).length)

      val resizedBody = truncateWhenUTF8(body, acceptableSize) + postfix

      new SafariPayloadBuilder[SetBody](
        payload.copy(custom = payload.custom.updated("body", resizedBody))
      )
    }
  }

  /**
   * Shrinks body so that the resulting payload message fits within require Apple specification (256 bytes)
   * @param setBody
   * @return
   */
  def shrinkBody(implicit setBody: ThisPayload =:= PayloadWithBody): SafariPayloadBuilder[SetBody] = shrinkBody("")

  /**
   * Shrinks body so that the resulting payload message fits within require Apple specification (256 bytes)
   * @param postfix for the truncated body, e.g. "..."
   * @param setBody
   * @return
   */
  def shrinkBody(postfix: String)(implicit setBody: ThisPayload =:= PayloadWithBody): SafariPayloadBuilder[SetBody] =
    resizeBody(maxPayloadLength, postfix)

  /**
   * Return length of payload bytes once marshaled to bytes
   * @return
   */
  def length: Int = copy.buildBytes.length

  /**
   * Return copy of this builder
   * @tparam A
   * @return
   */
  def copy[A <: HasBody]: SafariPayloadBuilder[A] = new SafariPayloadBuilder[A](payload.copy())

  override def toString: String = build

}
