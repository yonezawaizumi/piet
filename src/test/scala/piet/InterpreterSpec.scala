import java.io.{ByteArrayInputStream,ByteArrayOutputStream}

class InterpretorSpec extends munit.FunSuite {
  // FROM: https://ja.wikipedia.org/wiki/Piet
  test("ハロワ") {
    val input = getClass.getResourceAsStream("/hello_world.png")
    val is = new ByteArrayInputStream("".getBytes)
    val os = new ByteArrayOutputStream

    val colors = Colors(input, 11)

    val interpreter = new Interpreter(colors, is, os)
    
    interpreter.exec()
    assertEquals(os.toString, "Hello, world!\n")
  }
  // FROM: https://www.dangermouse.net/esoteric/piet/samples.html
  test("巨大ハロワ") {
    val input = getClass.getResourceAsStream("/hello_world_big.png")
    val is = new ByteArrayInputStream("".getBytes)
    val os = new ByteArrayOutputStream

    val colors = Colors(input, 5)

    val interpreter = new Interpreter(colors, is, os)
    
    interpreter.exec()
    assertEquals(os.toString, "Hello world!")
  }
  // FROM: https://www.dangermouse.net/esoteric/piet/samples.html
  test("地球儀ハロワ") {
    val input = getClass.getResourceAsStream("/world_hello_world.png")
    val is = new ByteArrayInputStream("".getBytes)
    val os = new ByteArrayOutputStream

    val colors = Colors(input, 5)

    val interpreter = new Interpreter(colors, is, os)
    
    interpreter.exec()
    assertEquals(os.toString, "Hello, world!\n")
  }
  // FROM: https://www.dangermouse.net/esoteric/piet/samples.html
  test("マルチコードサイズハロワ") {
    val input = getClass.getResourceAsStream("/helloworld-pietbig.png")
    val is = new ByteArrayInputStream("".getBytes)
    val os = new ByteArrayOutputStream

    val colors = Colors(input, 4)

    val interpreter = new Interpreter(colors, is, os)
    
    interpreter.debugExec()
    assertEquals(os.toString, "Hello world!\n")
  }
  test("マルチコードサイズピエト") {
    val input = getClass.getResourceAsStream("/helloworld-pietbig.png")
    val is = new ByteArrayInputStream("".getBytes)
    val os = new ByteArrayOutputStream

    val colors = Colors(input, 8)

    val interpreter = new Interpreter(colors, is, os)
    
    interpreter.debugExec()
    assertEquals(os.toString, "Piet\n")
  }
  // FROM: https://ymos-hobby-programing.hatenablog.com/entry/2023/02/04/153321
  test("10の階乗") {
    val input = getClass.getResourceAsStream("/factorial_10.png")
    val is = new ByteArrayInputStream("".getBytes)
    val os = new ByteArrayOutputStream

    val colors = Colors(input, 10)

    val interpreter = new Interpreter(colors, is, os)
    
    interpreter.exec()
    assertEquals(os.toString, "3628800")
  }
}
