sealed trait Direction {
  def value: Int
  val Right = 0
  val Down = 1
  val Left = 2
  val Up = 3
}

object Direction {
  object Right extends Direction { def value = Right }
  object Down extends Direction { def value = Down }
  object Left extends Direction { def value = Left }
  object Up extends Direction { def value = Up }
  def rotate(direction: Direction, times: Int) = (direction.value + times) % 4 match {
    case 0 => Right
    case 1 => Down
    case 2 => Left
    case 3 => Up
  }
}

sealed trait Chooser {
  def value: Int
  val Left = 0
  val Right = 1
}

object Chooser {
  object Left extends Chooser { def value = Left }
  object Right extends Chooser { def value = Right }
  def rotate(chooser: Chooser, times: Int) = (chooser.value + times) % 2 match {
    case 0 => Left
    case 1 => Right
  }
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
    if (hue == HueBlack || next.hue == HueBlack) {
      None
    } else if (hue == HueWhite || next.hue == HueWhite) {
      Some(Delta(0, 0))
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
