
import common._

import scala.util.Try

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

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
    val min = 0

    val lowLimitX = clamp(x - radius, min, src.width - 1)
    val highLimitX = clamp(x + radius, min, src.width - 1)

    val lowLimitY = clamp(y - radius, min, src.height - 1)
    val highLimitY = clamp(y + radius, min, src.height - 1)

    var numPixels = 1
    var sumRed = red(src(x, y))
    var sumGreen = green(src(x, y))
    var sumBlue = blue(src(x, y))
    var sumAlpha = alpha(src(x, y))

    for (xWalk <- lowLimitX to highLimitX) {
      for (yWalk <- lowLimitY to highLimitY) {
        if (xWalk != x || yWalk != y) {//skip identity
          val rgb = src(xWalk, yWalk)
          sumRed += red(rgb)
          sumGreen += green(rgb)
          sumBlue += blue(rgb)
          sumAlpha += alpha(rgb)
          numPixels += 1
        }
      }
    }

    def avg(value: Int) = Try(value / numPixels).getOrElse(0)

    rgba(avg(sumRed), avg(sumGreen), avg(sumBlue), avg(sumAlpha))
  }
}
