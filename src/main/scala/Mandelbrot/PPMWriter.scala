package Mandelbrot

import java.io._

/*
P6 -- magic number
# comment -- comment lines begin with #
# another comment -- any number of comment lines
200 300 -- image width & height
255 -- max color value
RGBRGBRGBRGBRGBRGB
*/

class PPMWriter(fname:String, imgWidth: Int, imgHeight: Int) {

  private var pixels:Int = imgWidth * imgHeight
  private val totalPixels:Int = pixels
  private val pw = new DataOutputStream(new FileOutputStream(fname))

  def width  = imgWidth
  def height = imgHeight

  writeHeader()

  private def writeHeader(): Unit = {
    pw.writeBytes("P6\u000a%d %d\u000a%d\u000a".format(imgWidth, imgHeight, 255))
  }

  def writePixel(r:Int, g:Int, b:Int):Unit = {
    pw.writeByte(r)
    pw.writeByte(g)
    pw.writeByte(b)
    pixels-=1
  }

  def writePixel(color:RGB):Unit = {
    writePixel(color.red,color.green,color.blue)
  }

  def done(): Unit = pw.close()

  def remainingPixels = pixels

  override def toString = s"Mandelbrot.PPMWriter - W:$imgWidth, H:$imgHeight, Remaining: $pixels / $totalPixels"
}
