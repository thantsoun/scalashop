
package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  type RGBTuple = (Int, Int, Int, Int)
  
  implicit class TupleAdd(t: RGBTuple) {
    def +(p: RGBTuple): (RGBA, RGBA, RGBA, RGBA) = (p._1 + t._1, p._2 + t._2, p._3 + t._3, p._4 + t._4)
  }

  val addTuple: (RGBTuple, RGBTuple) => RGBTuple = (a, b) => a + b
  
  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    
    val regionalValues: IndexedSeq[RGBTuple] = for {
      i <- x - radius to x + radius
      if i >= 0 && i < src.width
      j <- y - radius to y + radius
      if j >= 0 && j < src.height
    } yield (red(src(i, j)), green(src(i, j)), blue(src(i, j)), alpha(src(i, j)))

    val accValues = regionalValues.foldLeft((0, 0, 0, 0))(addTuple)
    val pixels = regionalValues.size
    
    rgba(accValues._1 / pixels, accValues._2 / pixels, accValues._3 / pixels, accValues._4 / pixels)
  }

}
