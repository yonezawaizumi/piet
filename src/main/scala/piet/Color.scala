object Direction {
  val Up = 0
  val Right = 1
  val Down = 2
  val Left = 3
}

object CodelDirection {
  val Right = 1
  val Left = 2
}

case class Delta(hue: Int, brightness: Int)

sealed trait Color {
  def code: String
  def hue: Int
  def brightness: Int
  val HueRed = 0
  val HueYellow = 1
  val HueGreen = 2
  val HueCyan = 3
  val HueBlue = 4
  val HueMagenta = 5
  val BrightnessLight = 0
  val BrightnessNormal = 1
  val BrightnessDark = 2
  val HueWhite = -1
  val HueBlack = -2

  def nextDelta(next: Color): Option[Delta] = {
    if (hue == HueWhite || hue == HueBlack || next.hue == HueWhite || next.hue == HueBlack) {
      None
    } else {
      Some(Delta((next.hue + 6 - hue) % 6, (next.brightness + 3 - brightness) % 3))
    }
  }
}

object Color {
  object LightRed extends Color {
    def code = "FFC0C0"
    def hue = HueRed
    def brightness = BrightnessLight
  }
  object Red extends Color {
    def code = "FF0000"
    def hue = HueRed
    def brightness = BrightnessNormal
  }
  object DarkRed extends Color {
    def code = "C00000"
    def hue = HueRed
    def brightness = BrightnessDark
  }
  object LightYellow extends Color {
    def code = "FFFFC0"
    def hue = HueYellow
    def brightness = BrightnessLight
  }
  object Yellow extends Color {
    def code = "FFFF00"
    def hue = HueYellow
    def brightness = BrightnessNormal
  }
  object DarkYellow extends Color {
    def code = "C0C000"
    def hue = HueYellow
    def brightness = BrightnessDark
  }
  object LightGreen extends Color {
    def code = "C0FFC0"
    def hue = HueGreen
    def brightness = BrightnessLight
  }
  object Green extends Color {
    def code = "00FF00"
    def hue = HueGreen
    def brightness = BrightnessNormal
  }
  object DarkGreen extends Color {
    def code = "00C000"
    def hue = HueGreen
    def brightness = BrightnessDark
  }
  object LightCyan extends Color {
    def code = "C0FFFF"
    def hue = HueCyan
    def brightness = BrightnessLight
  }
  object Cyan extends Color {
    def code = "00FFFF"
    def hue = HueCyan
    def brightness = BrightnessNormal
  }
  object DarkCyan extends Color {
    def code = "00C0C0"
    def hue = HueCyan
    def brightness = BrightnessDark
  }
  object LightBlue extends Color {
    def code = "C0C0FF"
    def hue = HueBlue
    def brightness = BrightnessLight
  }
  object Blue extends Color {
    def code = "0000FF"
    def hue = HueBlue
    def brightness = BrightnessNormal
  }
  object DarkBlue extends Color {
    def code = "0000C0"
    def hue = HueBlue
    def brightness = BrightnessDark
  }
  object LightMagenta extends Color {
    def code = "FFC0FF"
    def hue = HueMagenta
    def brightness = BrightnessLight
  }
  object Magenta extends Color {
    def code = "FF00FF"
    def hue = HueMagenta
    def brightness = BrightnessNormal
  }
  object DarkMagenta extends Color {
    def code = "C000C0"
    def hue = HueMagenta
    def brightness = BrightnessDark
  }
  object White extends Color {
    def code = "FFFFFF"
    def hue = HueWhite
    def brightness = BrightnessNormal
  }
  object Black extends Color {
    def code = "000000"
    def hue = HueBlack
    def brightness = BrightnessNormal
  }
}
