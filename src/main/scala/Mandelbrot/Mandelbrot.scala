package Mandelbrot



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

  def compute(location:Complex):Int = {
    compute(location, Complex.zero, 0, topOut)
  }

  def compute(cloc:Complex, zloc:Complex, iteration:Int, topOut:Int):Int = {
    if (zloc.modulus>2 || iteration >= topOut) {
      iteration
    } else {
      compute(cloc, zloc*zloc+cloc, iteration+1, topOut)
    }
  }


  def main(args: Array[String]) {

    lazy val grad = new GradientMaker(topOut, RGB(255,0,0), RGB(0,0,255))

    lazy val ppm = new PPMWriter("output.ppm", pixelWidth, pixelHeight)

    var row = 0
    var col = 0
    for(row <- 0 until pixelHeight) {
      print('.')
      val imgLoc  = top + (row * hpixelDelta)

      (0 until pixelWidth).foreach( col => {

        val realLoc = left + (col * wpixelDelta)

        val iterations = compute(Complex(realLoc, imgLoc))

        val color = grad.getStep(iterations)

        ppm.writePixel(color)
      })

    }

    ppm.done()
  }

}
