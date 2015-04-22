package com.github.puscala

import org.specs2.mutable.Specification

sealed trait PayloadBuilderSpecBase {

  def truncate(s: String): String = s.replaceAll("""\r|\n|\r\n""", "").replaceAll(" ", "")

  def strOfLength(l: Int): String = {
    val sb = new StringBuilder
    (0 until l).foreach(_ => sb.append('c'))

    sb.toString()
  }

}

class PayloadBuilderSpec extends Specification with PayloadBuilderSpecBase {

  private def payloadOf(l: Int): PayloadBuilder[SetBody] = PayloadBuilder().body(strOfLength(l))

  "PayloadBuilder" >> {

    "empty" >> {
      val expected = """{"aps":{}}"""
      val actual = PayloadBuilder().toString

      actual must be equalTo expected
    }

    "oneAps" >> {
      val expected = """{"aps":{"alert":"test"}}"""
      val actual = PayloadBuilder().body("test").toString

      actual must be equalTo expected
    }

    "twoAps" >> {
      val expected = """{"aps":{"badge":9,"alert":"test"}}"""
      val actual = PayloadBuilder().body("test").badge(9).toString

      actual must be equalTo expected
    }

    "twoApsMultipleBuilds" >> {
      val builder = PayloadBuilder().body("test").badge(9)
      val expected = """{"aps":{"badge":9,"alert":"test"}}"""

      builder.build must be equalTo expected
      builder.build must be equalTo expected
    }

    "includeBadge" >> {
      val expected = """{"aps":{"badge":0}}"""
      val actualBadge1 = PayloadBuilder().badge(0).toString
      val actualBadge2 = PayloadBuilder().badge(1).clearBadge.toString

      actualBadge1 must be equalTo expected
      actualBadge2 must be equalTo expected
    }

    "oneLocalizedWithArray" >> {
      val expected = truncate(
        """
          |{
          |  "aps":{
          |    "sound":"chime",
          |    "alert":{
          |      "loc-key":"GAME_PLAY_REQUEST_FORMAT",
          |      "loc-args":["Jenna","Frank"]
          |    }
          |  }
          |}
        """.stripMargin
      )
      val actualBuilder = PayloadBuilder().localizedKey("GAME_PLAY_REQUEST_FORMAT").localizedArgs("Jenna", "Frank")
      val actual = actualBuilder.sound("chime").toString

      actual must be equalTo expected
    }

    "customField" >> {
      val expected = """{"ache1":"what","ache2":2,"aps":{"alert":"test"}}"""
      val actual = PayloadBuilder().body("test").customField("ache1", "what").customField("ache2", 2).toString

      actual must be equalTo expected
    }

    "customFieldWithArray" >> {
      val expected = """{"ache1":["a1","a2"],"ache2":[1,2],"aps":{"alert":"test"}}"""
      val actual = PayloadBuilder().body("test").customField("ache1", List("a1", "a2")).customField("ache2", Seq(1, 2)).toString

      actual must be equalTo expected
    }

    "alertView" >> {
      val expected = """{"aps":{"alert":{"action-loc-key":"View","body":"what"}}}"""
      val actual = PayloadBuilder().actionKey(Some("View")).body("what").toString

      actual must be equalTo expected
    }

    "alertNoView" >> {
      val expected = """{"aps":{"alert":{"action-loc-key":null,"body":"what"}}}"""
      val actual = PayloadBuilder().actionKey(None).body("what").toString

      actual must be equalTo expected
    }

    "alertNoViewSimpler" >> {
      val expected = """{"aps":{"alert":{"action-loc-key":null,"body":"what"}}}"""
      val actual = PayloadBuilder().noActionButton.body("what").toString

      actual must be equalTo expected
    }

    "alertWithImageOnly" >> {
      val expected = """{"aps":{"alert":{"launch-image":"/test"}}}"""
      val actual = PayloadBuilder().launchImage("/test").toString

      actual must be equalTo expected
    }

    "alertWithImageAndText" >> {
      val expected = """{"aps":{"alert":{"launch-image":"/test","body":"hello"}}}"""
      val actual = PayloadBuilder().launchImage("/test").body("hello").toString

      actual must be equalTo expected
    }

    "bitComplicated" >> {
      val expected = truncate(
        """
          |{
          |  "achme":"foo",
          |  "aps":{
          |    "sound":"chime",
          |    "alert":{
          |      "loc-key":"GAME_PLAY_REQUEST_FORMAT",
          |      "loc-args":["Jenna","Frank"]
          |    }
          |  }
          |}
        """.stripMargin
      )
      val actual =
        PayloadBuilder().
          customField("achme", "foo").
          sound("chime").
          localizedKey("GAME_PLAY_REQUEST_FORMAT").
          localizedArgs("Jenna", "Frank").toString

      actual must be equalTo expected
    }

    "copyReturnsNewInstance" >> {
      val expected = """{"aps":{"sound":"chime"}}"""
      val builder = PayloadBuilder().sound("chime")
      val actual = builder.toString

      actual must be equalTo expected

      val copyExpected = """{"aps":{"sound":"chime","badge":5}}"""
      val copyActual = builder.copy.badge(5).toString

      copyActual must be equalTo copyExpected
    }

    "simpleEnglishLength" >> {
      val builder = PayloadBuilder().body("test")
      val expected = """{"aps":{"alert":"test"}}"""

      builder.build must be equalTo expected

      val actualLength = Utilities.toUTF8Bytes(expected).length

      builder.length must be equalTo actualLength
      builder.isTooLong must beFalse
    }

    "detectLongMessage" >> {
      val maxLength = 2048
      val basic = """{"aps":{"alert":""}}"""
      val wrapperOverhead = basic.length
      val cutoffForAlert = maxLength - wrapperOverhead

      val tooShort = payloadOf(1)
      tooShort.isTooLong must beFalse
      tooShort.length must be equalTo wrapperOverhead + 1

      val bitShort = payloadOf(cutoffForAlert - 1)
      bitShort.isTooLong must beFalse
      bitShort.length must be equalTo wrapperOverhead + cutoffForAlert - 1

      val border = payloadOf(cutoffForAlert)
      border.isTooLong must beFalse
      border.length must be equalTo wrapperOverhead + cutoffForAlert
      border.length must be equalTo maxLength

      val bitLong = payloadOf(cutoffForAlert + 1)
      bitLong.isTooLong must beTrue
      bitLong.length must be equalTo wrapperOverhead + cutoffForAlert + 1

      val tooLong = payloadOf(cutoffForAlert + 1000)
      tooLong.isTooLong must beTrue
      tooLong.length must be equalTo wrapperOverhead + cutoffForAlert + 1000
    }

    "shrinkLongMessages" >> {
      val maxLength = 2048
      val basic = """{"aps":{"alert":""}}"""
      val wrapperOverhead = basic.length
      val cutoffForAlert = maxLength - wrapperOverhead

      val tooShort = payloadOf(1).shrinkBody
      tooShort.isTooLong must beFalse
      tooShort.length must be equalTo wrapperOverhead + 1

      val bitShort = payloadOf(cutoffForAlert - 1).shrinkBody
      bitShort.isTooLong must beFalse
      bitShort.length must be equalTo wrapperOverhead + cutoffForAlert - 1

      val border = payloadOf(cutoffForAlert).shrinkBody
      border.isTooLong must beFalse
      border.length must be equalTo maxLength

      val bitLong = payloadOf(cutoffForAlert + 1).shrinkBody
      bitLong.isTooLong must beFalse
      bitLong.length must be equalTo maxLength

      val tooLong = payloadOf(cutoffForAlert + 1000).shrinkBody
      tooLong.isTooLong must beFalse
      tooLong.length must be equalTo maxLength
    }

    "shrinkLongMessagesWithOtherField" >> {
      val maxLength = 2048
      val basic = """{"aps":{"alert":""}}"""
      val wrapperOverhead = basic.length
      val cutoffForAlert = maxLength - wrapperOverhead

      val tooShort = payloadOf(1).sound("default").shrinkBody
      tooShort.isTooLong must beFalse
      tooShort.length < maxLength must beTrue

      val bitShort = payloadOf(cutoffForAlert - 1).sound("default").shrinkBody
      bitShort.isTooLong must beFalse
      bitShort.length must be equalTo maxLength

      val border = payloadOf(cutoffForAlert).sound("default").shrinkBody
      border.isTooLong must beFalse
      border.length must be equalTo maxLength

      val bitLong = payloadOf(cutoffForAlert + 1).sound("default").shrinkBody
      bitLong.isTooLong must beFalse
      bitLong.length must be equalTo maxLength

      val tooLong = payloadOf(cutoffForAlert + 1000).sound("default").shrinkBody
      tooLong.isTooLong must beFalse
      tooLong.length must be equalTo maxLength
    }

    "supportMDM" >> {
      val expected = """{"mdm":"123"}"""
      val actual = PayloadBuilder().mdm("123").toString

      actual must be equalTo expected
    }

    "mdmWithOtherField" >> {
      val expected = """{"mdm":"123"}"""
      val actual = PayloadBuilder().body("test").mdm("123").toString

      actual must be equalTo expected
    }

    "supportNewsstand" >> {
      val expected = """{"aps":{"content-available":1}}"""
      val actual = PayloadBuilder().forNewsstand.toString

      actual must be equalTo expected
    }

    "utf8Encoding" >> {
      val body = "ボディー"
      val payload = PayloadBuilder().body(body).toString

      payload.indexOf(body) >= 0 must beTrue
    }

    "utf8EncodingByEscaped" >> {
      val body = "body\u00E9"
      val payload = PayloadBuilder().body(body).toString

      payload.indexOf(body) >= 0 must beTrue
    }

    "supportBackground" >> {
      val expected = """{"aps":{"content-available":1}}"""
      val actual = PayloadBuilder().forBackground.toString

      actual must be equalTo expected
    }

    "backgroundWithCustomKey" >> {
      val expected = """{"ache1":"what","aps":{"content-available":1}}"""
      val actual = PayloadBuilder().forBackground.customField("ache1", "what").toString

      actual must be equalTo expected
    }

    "backgroundWithAlert" >> {
      val expected = """{"aps":{"content-available":1,"alert":"test"}}"""
      val actual = PayloadBuilder().body("test").forBackground.toString

      actual must be equalTo expected
    }

  }

}

class SafariPayloadBuilderSpec extends Specification with PayloadBuilderSpecBase {

  private def payloadOf(l: Int): SafariPayloadBuilder[SetBody] = SafariPayloadBuilder().body(strOfLength(l))

  "SafariPayloadBuilder" >> {

    "safariAps" >> {
      val expected = truncate(
        """
          |{
          |  "aps":{
          |    "url-args":["arg1","arg2","arg3"],
          |    "alert":{
          |      "body":"test",
          |      "title":"TestTitle"
          |    }
          |  }
          |}
        """.stripMargin
      )

      val actual =
        SafariPayloadBuilder().body("test").title("TestTitle").urlArgs("arg1", "arg2", "arg3").toString


      actual must be equalTo expected
    }

    "detectLongMessage" >> {
      val maxLength = 256
      val basic = """{"aps":{"alert":""}}"""
      val wrapperOverhead = basic.length
      val cutoffForAlert = maxLength - wrapperOverhead

      val tooShort = payloadOf(1)
      tooShort.isTooLong must beFalse
      tooShort.length must be equalTo wrapperOverhead + 1

      val bitShort = payloadOf(cutoffForAlert - 1)
      bitShort.isTooLong must beFalse
      bitShort.length must be equalTo wrapperOverhead + cutoffForAlert - 1

      val border = payloadOf(cutoffForAlert)
      border.isTooLong must beFalse
      border.length must be equalTo wrapperOverhead + cutoffForAlert
      border.length must be equalTo maxLength

      val bitLong = payloadOf(cutoffForAlert + 1)
      bitLong.isTooLong must beTrue
      bitLong.length must be equalTo wrapperOverhead + cutoffForAlert + 1

      val tooLong = payloadOf(cutoffForAlert + 1000)
      tooLong.isTooLong must beTrue
      tooLong.length must be equalTo wrapperOverhead + cutoffForAlert + 1000
    }

    "shrinkLongMessages" >> {
      val maxLength = 256
      val basic = """{"aps":{"alert":""}}"""
      val wrapperOverhead = basic.length
      val cutoffForAlert = maxLength - wrapperOverhead

      val tooShort = payloadOf(1).shrinkBody
      tooShort.isTooLong must beFalse
      tooShort.length must be equalTo wrapperOverhead + 1

      val bitShort = payloadOf(cutoffForAlert - 1).shrinkBody
      bitShort.isTooLong must beFalse
      bitShort.length must be equalTo wrapperOverhead + cutoffForAlert - 1

      val border = payloadOf(cutoffForAlert).shrinkBody
      border.isTooLong must beFalse
      border.length must be equalTo maxLength

      val bitLong = payloadOf(cutoffForAlert + 1).shrinkBody
      bitLong.isTooLong must beFalse
      bitLong.length must be equalTo maxLength

      val tooLong = payloadOf(cutoffForAlert + 1000).shrinkBody
      tooLong.isTooLong must beFalse
      tooLong.length must be equalTo maxLength
    }

    "shrinkLongMessagesWithOtherField" >> {
      val maxLength = 256
      val basic = """{"aps":{"alert":""}}"""
      val wrapperOverhead = basic.length
      val cutoffForAlert = maxLength - wrapperOverhead

      val tooShort = payloadOf(1).title("default").shrinkBody
      tooShort.isTooLong must beFalse
      tooShort.length < maxLength must beTrue

      val bitShort = payloadOf(cutoffForAlert - 1).title("default").shrinkBody
      bitShort.isTooLong must beFalse
      bitShort.length must be equalTo maxLength

      val border = payloadOf(cutoffForAlert).title("default").shrinkBody
      border.isTooLong must beFalse
      border.length must be equalTo maxLength

      val bitLong = payloadOf(cutoffForAlert + 1).title("default").shrinkBody
      bitLong.isTooLong must beFalse
      bitLong.length must be equalTo maxLength

      val tooLong = payloadOf(cutoffForAlert + 1000).title("default").shrinkBody
      tooLong.isTooLong must beFalse
      tooLong.length must be equalTo maxLength
    }

  }

}
