import java.io.{ByteArrayInputStream,ByteArrayOutputStream}

class InterpretorSpec extends munit.FunSuite {
  /*test("ハロワ") {
    val input = getClass.getResourceAsStream("/hello_world.png")
    val is = new ByteArrayInputStream("".getBytes)
    val os = new ByteArrayOutputStream

    val colors = Colors(input, 11)

    val interpreter = new Interpreter(colors, is, os)
    
    interpreter.exec()
    assertEquals(os.toString, "Hello, world!\n")
  }
  test("巨大ハロワ") {
    val input = getClass.getResourceAsStream("/hello_world_big.png")
    val is = new ByteArrayInputStream("".getBytes)
    val os = new ByteArrayOutputStream

    val colors = Colors(input, 5)

    val interpreter = new Interpreter(colors, is, os)
    
    interpreter.exec()
    assertEquals(os.toString, "Hello world!")
  }*/
  test("10の階乗") {
    val input = getClass.getResourceAsStream("/factorial_10.png")
    val is = new ByteArrayInputStream("".getBytes)
    val os = new ByteArrayOutputStream

    val colors = Colors(input, 10)

    val interpreter = new Interpreter(colors, is, os)
    
    interpreter.debugExec()
    assertEquals(os.toString, "3628800")
  }
}
