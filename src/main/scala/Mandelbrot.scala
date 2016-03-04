object Mandelbrot {

  private val left:Double   = -2
  private val top:Double    = -1
  private val right:Double  = 2
  private val bottom:Double = 1
  private val pixelWidth    = 800
  private val pixelHeight   = 400

  private val width:Double  = Math.abs(left - right)
  private val height:Double = Math.abs(top - bottom)
  private val wpixelDelta:Double = width/pixelWidth
  private val hpixelDelta:Double = height/pixelHeight

  private val topOut = 10000

  def main(args: Array[String]) {

    val grad = new GradientMaker(topOut, RGB(255,0,0), RGB(0,0,255))
    val ppm = new PPMWriter("output.ppm", pixelWidth, pixelHeight)

    var row = 0
    var col = 0
    for(row <- 0 until pixelHeight) {
      print('.')
      for(col <- 0 until pixelWidth) {

        val realLoc = left + (col * wpixelDelta)
        val imgLoc  = top + (row * hpixelDelta)

        val valueC = Complex(realLoc, imgLoc)
        var valueZ = Complex(0,0)
        var iterations = 0

        while (valueZ.modulus < 2 && iterations < topOut-1) {
          valueZ = valueZ * valueZ + valueC
          iterations+=1
        }

        val color = grad.getStep(iterations)

        ppm.writePixel(color)
      }
    }

    ppm.done()
  }

}
