class ColorSpec extends munit.FunSuite {
  test("赤") {
    val color = Color.Red
    assertEquals(color.code, "FF0000")
    assertEquals(color.hue, color.HueRed)
    assertEquals(color.brightness, color.BrightnessNormal)
  }
  test("差分") {
    assertEquals(Color.Red.nextDelta(Color.DarkRed), Some(Delta(0, 1)))
    assertEquals(Color.DarkRed.nextDelta(Color.Red), Some(Delta(0, 2)))
    assertEquals(Color.Red.nextDelta(Color.Magenta), Some(Delta(5, 0)))
    assertEquals(Color.Magenta.nextDelta(Color.Red), Some(Delta(1, 0)))
  }
}
