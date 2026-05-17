import java.io.{IOException, InputStream}
import javax.imageio.ImageIO

// NOTE: 1つの正方形（コーデル）が複数の色を含んでいたときの例外。座標はコーデル単位
case class MultiColorCodelException(x: Int, y: Int)
  extends RuntimeException(s"codel at ($x, $y) contains multiple colors")

case class BlockStatus(x: Int, y: Int, color: Color, size: Int)

// NOTE: PNG から読み込んだコーデルの2次元配列
// colors は immutable な Array として private に保持する
class Colors(private val colors: Array[Color], val width: Int, val height: Int) {
  // NOTE: (x, y) のコーデルの色を返す。範囲外なら例外を投げる
  def get(x: Int, y: Int): Color = {
    if (x < 0 || x >= width || y < 0 || y >= height) {
      throw new IndexOutOfBoundsException(s"($x, $y) is out of range (width: $width, height: $height)")
    }
    colors(y * width + x)
  }

  // NOTE: direction を (dx, dy) ベクトルに変換
  private def directionDelta(direction: Direction): (Int, Int) = direction match {
    case Direction.Right => (1, 0)
    case Direction.Left  => (-1, 0)
    case Direction.Down  => (0, 1)
    case Direction.Up    => (0, -1)
  }

  // NOTE: 次のコーデルの座標を得る
  def nextCodel(x: Int, y: Int, direction: Direction): Option[(Int, Int)] = {
    val delta = directionDelta(direction)
    val (nx, ny) = (x + delta._1, y + delta._2)
    if (nx < 0 || width - 1 < nx || ny < 0 || height - 1 < ny) None else Some((nx, ny))
  }

  // NOTE: (x, y) を含むカラーブロックを direction の向きに抜けた直後の座標と、ブロックサイズを返す
  //   カラーブロック: 指定されたコーデルと辺で隣接する、同じ色を持つコーデルのかたまり
  //   端コーデル: カラーブロックのうち、direction で指定された向きで最も遠方にあるコーデルのうち、その向きを向いたときの chooser で指定された向きにある遠方のコーデル
  //   端コーデルから direction に 1 つ進んだ座標を Some で返す。画像をはみ出す場合は None
  //   不正な direction / chooser は例外スロー
  def getBlock(x: Int, y: Int, direction: Direction, chooser: Chooser): Option[BlockStatus] = {
    if (direction != Direction.Up && direction != Direction.Right
        && direction != Direction.Down && direction != Direction.Left) {
      throw new IllegalArgumentException(s"invalid direction: $direction")
    }
    if (chooser != Chooser.Left && chooser != Chooser.Right) {
      throw new IllegalArgumentException(s"invalid chooser: $chooser")
    }

    val color = get(x, y)
    // NOTE: 4近傍 BFS で同色の連結成分を集める
    @scala.annotation.tailrec
    def bfs(queue: List[(Int, Int)], visited: Set[(Int, Int)]): Set[(Int, Int)] = queue match {
      case Nil => visited
      case (cx, cy) :: rest =>
        val next = List((cx + 1, cy), (cx - 1, cy), (cx, cy + 1), (cx, cy - 1)).filter {
          case (nx, ny) =>
            nx >= 0 && nx < width && ny >= 0 && ny < height &&
              !visited.contains((nx, ny)) && get(nx, ny) == color
        }
        bfs(rest ++ next, visited ++ next)
    }
    val codels = bfs(List((x, y)), Set((x, y))).toSeq

    // NOTE: direction で最遠の端コーデル群を抽出
    val edge = direction match {
      case Direction.Right => val m = codels.map(_._1).max; codels.filter(_._1 == m)
      case Direction.Left  => val m = codels.map(_._1).min; codels.filter(_._1 == m)
      case Direction.Down  => val m = codels.map(_._2).max; codels.filter(_._2 == m)
      case Direction.Up    => val m = codels.map(_._2).min; codels.filter(_._2 == m)
    }

    // NOTE: chooser で上の集合から 1つ選ぶ
    val (ex, ey) = (direction, chooser) match {
      case (Direction.Right, Chooser.Left)  => edge.minBy(_._2)
      case (Direction.Right, Chooser.Right) => edge.maxBy(_._2)
      case (Direction.Down,  Chooser.Left)  => edge.maxBy(_._1)
      case (Direction.Down,  Chooser.Right) => edge.minBy(_._1)
      case (Direction.Left,  Chooser.Left)  => edge.maxBy(_._2)
      case (Direction.Left,  Chooser.Right) => edge.minBy(_._2)
      case (Direction.Up,    Chooser.Left)  => edge.minBy(_._1)
      case (Direction.Up,    Chooser.Right) => edge.maxBy(_._1)
    }

    // NOTE: 端コーデルから direction に 1 つ進んだ座標。画像をはみ出すなら None
    val (dx, dy) = directionDelta(direction)
    val nx = ex + dx
    val ny = ey + dy
    if (nx < 0 || nx >= width || ny < 0 || ny >= height) None
    else Some(BlockStatus(nx, ny, get(nx, ny), codels.size))
  }

  // NOTE: (x, y) が White のとき、direction の向きに白のまま直進した最終位置を返す
  //   非Whiteのコーデル、または画像のふちに到達した直前の白コーデルの座標
  //   (x, y) が White でない場合、または direction が不正な場合は例外
  def getWhiteBlock(x: Int, y: Int, direction: Direction): (Int, Int) = {
    if (direction != Direction.Up && direction != Direction.Right
        && direction != Direction.Down && direction != Direction.Left) {
      throw new IllegalArgumentException(s"invalid direction: $direction")
    }
    if (get(x, y) != Color.White) {
      throw new IllegalArgumentException(s"codel at ($x, $y) is not White")
    }

    val (dx, dy) = directionDelta(direction)

    @scala.annotation.tailrec
    def walk(cx: Int, cy: Int): (Int, Int) = {
      val nx = cx + dx
      val ny = cy + dy
      if (nx < 0 || nx >= width || ny < 0 || ny >= height || get(nx, ny) != Color.White) {
        (cx, cy)
      } else {
        walk(nx, ny)
      }
    }
    walk(x, y)
  }
}

object Colors {
  // NOTE: 色コードから Color オブジェクトを引くための表
  private val code2Color: Map[String, Color] = Seq(
    Color.LightRed, Color.Red, Color.DarkRed,
    Color.LightYellow, Color.Yellow, Color.DarkYellow,
    Color.LightGreen, Color.Green, Color.DarkGreen,
    Color.LightCyan, Color.Cyan, Color.DarkCyan,
    Color.LightBlue, Color.Blue, Color.DarkBlue,
    Color.LightMagenta, Color.Magenta, Color.DarkMagenta,
    Color.White, Color.Black
  ).map(color => color.code -> color).toMap

  // NOTE: PNG の InputStream を pixelsPerCodel 四方の正方形に分割して Colors を作る
  def apply(input: InputStream, pixelsPerCodel: Int = 1): Colors = {
    require(pixelsPerCodel > 0, s"pixelsPerCodel must be positive: $pixelsPerCodel")

    // NOTE: PNG を読み込む。読み込みに失敗したら例外を投げる
    val image = try {
      ImageIO.read(input)
    } catch {
      case e: IOException => throw new IllegalArgumentException("failed to read PNG stream", e)
    }
    if (image == null) {
      throw new IllegalArgumentException("failed to read PNG stream")
    }

    // NOTE: 縦横が pixelsPerCodel の倍数でなければ分割できないので例外を投げる
    if (image.getWidth % pixelsPerCodel != 0 || image.getHeight % pixelsPerCodel != 0) {
      throw new IllegalArgumentException(
        s"image size (${image.getWidth} x ${image.getHeight}) is not a multiple of pixelsPerCodel ($pixelsPerCodel)"
      )
    }

    val width = image.getWidth / pixelsPerCodel
    val height = image.getHeight / pixelsPerCodel

    // NOTE: 左上から右下へ水平に走査して、コーデルごとの色を求める
    val colors = for {
      codelY <- 0 until height
      codelX <- 0 until width
    } yield {
      val baseX = codelX * pixelsPerCodel
      val baseY = codelY * pixelsPerCodel
      // NOTE: 正方形の左上の色を取得（アルファ値は無視）
      val rgb = image.getRGB(baseX, baseY) & 0XFFFFFF
      // NOTE: 異なる色が同じコーデル内にある場合、例外を投げる
      if ((for {
        offsetY <- 0 until pixelsPerCodel
        offsetX <- 0 until pixelsPerCodel
      } yield image.getRGB(baseX + offsetX, baseY + offsetY) & 0xFFFFFF).exists(_ != rgb)) {
        throw MultiColorCodelException(codelX, codelY)
      }
      // NOTE: 色コードに一致する Color へ。一致しなければ Color.Black
      code2Color.getOrElse(f"${rgb}%06X", Color.Black)
    }

    new Colors(colors.toArray, width, height)
  }
}
