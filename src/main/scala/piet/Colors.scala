import java.io.{IOException, InputStream}
import javax.imageio.ImageIO

// NOTE: 1つの正方形（コーデル）が複数の色を含んでいたときの例外。座標はコーデル単位
case class MultiColorCodelException(x: Int, y: Int)
  extends RuntimeException(s"codel at ($x, $y) contains multiple colors")

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

  // NOTE: (x, y) を含むカラーブロックの端コーデルとサイズを返す
  //   カラーブロック: 指定されたコーデルと辺で隣接する、同じ色を持つコーデルのかたまり
  //   橋コーデル: カラーブロックのうち、direction で指定された向きで最も遠方にあるコーデルのうち、その向きを向いたときの codelDirection で指定された向きにある遠方のコーデル
  //   不正な direction / codelDirection は例外スロー
  def getBlock(x: Int, y: Int, direction: Int, codelDirection: Int): (Int, Int, Int) = {
    if (direction != Direction.Up && direction != Direction.Right
        && direction != Direction.Down && direction != Direction.Left) {
      throw new IllegalArgumentException(s"invalid direction: $direction")
    }
    if (codelDirection != CodelDirection.Left && codelDirection != CodelDirection.Right) {
      throw new IllegalArgumentException(s"invalid codelDirection: $codelDirection")
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

    // NOTE: codelDirection で上の集合から 1つ選ぶ
    val (ex, ey) = (direction, codelDirection) match {
      case (Direction.Right, CodelDirection.Left)  => edge.minBy(_._2)
      case (Direction.Right, CodelDirection.Right) => edge.maxBy(_._2)
      case (Direction.Down,  CodelDirection.Left)  => edge.maxBy(_._1)
      case (Direction.Down,  CodelDirection.Right) => edge.minBy(_._1)
      case (Direction.Left,  CodelDirection.Left)  => edge.maxBy(_._2)
      case (Direction.Left,  CodelDirection.Right) => edge.minBy(_._2)
      case (Direction.Up,    CodelDirection.Left)  => edge.minBy(_._1)
      case (Direction.Up,    CodelDirection.Right) => edge.maxBy(_._1)
    }

    (ex, ey, codels.size)
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
