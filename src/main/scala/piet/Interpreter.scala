import java.io.{InputStream, PushbackInputStream, OutputStream}
import java.lang.Character

sealed case class State(
  x :Int = 0,
  y :Int = 0,
  directionPointer: Direction = Direction.Right,
  codelChooser: Chooser = Chooser.Left,
  stack: List[Int] = List.empty[Int] // NOTE: 先頭に積む・先頭から取る
)

class Interpreter(colors: Colors, _input: InputStream, output: OutputStream) {
  private val input = new PushbackInputStream(_input)

  // NOTE: ROLL
  private def roll(stack: List[Int]): List[Int] = {
    val times = stack.head
    val depth = stack.tail.head
    val s = stack.tail.tail
    // NOTE: 深さが非正、または残りのスタック（長さ-2）
    if (times == 0 || depth <= 0 || s.lengthCompare(depth) <= 0) s
    else {
      val (t, b) = s.splitAt(depth)
      // NOTE: t は空でない
      val k = ((times % depth) + depth) % depth
      t.drop(k) ::: t.take(k) ::: b
    }  
  }

  // NOTE: IN(number)
  private def inNumber(): Option[Int] = {
    val (chars, next) = Iterator.continually(input.read()).span(c => Character.isDigit(c))
    next.next() match {
      case -1 => None // NOTE: EOF
      case c =>
        input.unread(c)
        try {
          Some(Integer.parseInt(chars.mkString))
        } catch {
          case e: NumberFormatException => None
        }
    }
  }

  def next(state: State): Option[State] = {
    colors.get(state.x, state.y) match {
      case Color.White => 
        // NOTE: 白は命令をスキップする
        // NOTE: 白で無限ループする際の仕様を npiet も満たしていないらしく、この時点での無限ループでのみ停止するものとする
        (0 to 3).iterator.map(i => {
          val dp = Direction.rotate(state.directionPointer, i)
          val cc = Chooser.rotate(state.codelChooser, i)
          val (x, y) = colors.getWhiteBlock(state.x, state.y, dp)
          colors.nextCodel(x, y, dp).flatMap(xy => if (colors.get(xy._1, xy._2) != Color.Black) Some(State(xy._1, xy._2, dp, cc, state.stack)) else None)
        }).find(_.isDefined).flatten
      case color: Color =>
        (0 to 7).iterator.map(i => {
          // dp は 2回め、4回め、…に加算
          val dp = Direction.rotate(state.directionPointer, i / 2)
          // cc は 1回め、3回め、に加算
          val cc = Chooser.rotate(state.codelChooser, (i + 1) / 2)
          colors.getBlock(state.x, state.y, dp, cc).filter(_.color != Color.Black).flatMap(next => color.nextDelta(next.color).map(_ match {
            case Delta(0, 0) => // NOTE: NOOP (白ブロック)
              State(next.x, next.y, dp, cc, state.stack)
            case Delta(0, 1) => // NOTE: PUSH
              State(next.x, next.y, dp, cc, next.size +: state.stack)
            case Delta(0, 2) => // NOTE: POP
              State(next.x, next.y, dp, cc, if (state.stack.isEmpty) state.stack else state.stack.tail)
            case Delta(1, 0) => // NOTE: ADD
              State(next.x, next.y, dp, cc, if (state.stack.lengthCompare(2) < 0) state.stack else (state.stack.tail.head + state.stack.head) +: state.stack.tail.tail)
            case Delta(1, 1) => // NOTE: SUBTRACT
              State(next.x, next.y, dp, cc, if (state.stack.lengthCompare(2) < 0) state.stack else (state.stack.tail.head - state.stack.head) +: state.stack.tail.tail)
            case Delta(1, 2) => // NOTE: MULTIPLY
              State(next.x, next.y, dp, cc, if (state.stack.lengthCompare(2) < 0) state.stack else (state.stack.tail.head * state.stack.head) +: state.stack.tail.tail)
            case Delta(2, 0) => // NOTE: DEVIDE
              State(next.x, next.y, dp, cc, if (state.stack.lengthCompare(2) < 0 || state.stack.head == 0) state.stack else (state.stack.tail.head / state.stack.head) +: state.stack.tail.tail)
            case Delta(2, 1) => // NOTE: MOD
              State(next.x, next.y, dp, cc, if (state.stack.lengthCompare(2) < 0 || state.stack.head == 0) state.stack else (state.stack.tail.head % state.stack.head) +: state.stack.tail.tail)
            case Delta(2, 2) => // NOTE: NOT
              State(next.x, next.y, dp, cc, if (state.stack.isEmpty) state.stack else (if (state.stack.head == 0) 1 else 0) +: state.stack.tail)
            case Delta(3, 0) => // NOTE: GREATE
              State(next.x, next.y, dp, cc, if (state.stack.lengthCompare(2) < 0) state.stack else(if (state.stack.tail.head > state.stack.head) 1 else 0) +: state.stack.tail.tail)
            case Delta(3, 1) => // NOTE: POINTER
              if (state.stack.isEmpty) State(next.x, next.y, dp, cc, state.stack)
              else State(next.x, next.y, Direction.rotate(dp, state.stack.head), cc, state.stack.tail)
            case Delta(3, 2) => // NOTE: SWITCH
              if (state.stack.isEmpty) State(next.x, next.y, dp, cc, state.stack)
              else State(next.x, next.y, dp, Chooser.rotate(cc, state.stack.head), state.stack.tail)
            case Delta(4, 0) => // NOTE: DUPLICATE
              State(next.x, next.y, dp, cc, if (state.stack.isEmpty) state.stack else state.stack.head +: state.stack)
            case Delta(4, 1) => // NOTE: ROLL
              State(next.x, next.y, dp, cc, if (state.stack.lengthCompare(2) < 0) state.stack else roll(state.stack))
            case Delta(4, 2) => // NOTE: IN(number)
              State(next.x, next.y, dp, cc, inNumber() match {
                case None => state.stack
                case Some(n) => n +: state.stack
              })
            case Delta(5, 0) => // NOTE: IN(char)
              State(next.x, next.y, dp, cc, input.read() match {
                case -1 => state.stack // EOF
                case c => c +: state.stack
              })
            case Delta(5, 1) => // NOTE: OUT(number)
              State(next.x, next.y, dp, cc, if (state.stack.isEmpty) state.stack else {
                val n = state.stack.head
                output.write(f"$n".getBytes)
                state.stack.tail
              })
            case Delta(5, 2) => // NOTE: OUT(char)
              State(next.x, next.y, dp, cc, if (state.stack.isEmpty) state.stack else {
                output.write(state.stack.head)
                state.stack.tail
              })
            case _ => throw new IllegalArgumentException("Delta is impossible")
            }
          )
        )}).find(_.isDefined).flatten
    }
  }

  @scala.annotation.tailrec
  final def exec(state: Option[State] = Some(State())): Option[State] = state match {
    case None => None
    case Some(state) => exec(next(state))
  }

  @scala.annotation.tailrec
  final def debugExec(state: Option[State] = Some(State())): Option[State] = state match {
    case None => None
    case Some(state) =>
      println(state.toString)
      println(output.toString)
      debugExec(next(state))
  }
}
