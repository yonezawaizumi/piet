class ColorsSpec extends munit.FunSuite {

  test("赤") {
    val color = Color.Red
    assertEquals(color.code, "FF0000")
    assertEquals(color.hue, color.HueRed)
    assertEquals(color.brightness, color.BrightnessNormal)
  }

  test("80x20") {
    val input = getClass.getResourceAsStream("/60x40.png")
    // 初期化時に例外がスローされないこと
    val colors = Colors(input, 20)
    input.close()
    assertEquals(colors.width, 3)
    assertEquals(colors.height, 2)
    assertEquals(colors.get(0, 0), Color.Yellow)
    assertEquals(colors.get(1, 0), Color.Black)
    assertEquals(colors.get(2, 0), Color.Black) // 解釈できない色
    assertEquals(colors.get(0, 1), Color.Black)
    assertEquals(colors.get(1, 1), Color.White)
    assertEquals(colors.get(2, 1), Color.Red)
  }
}
