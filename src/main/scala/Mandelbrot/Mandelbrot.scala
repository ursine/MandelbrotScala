package Mandelbrot

import scala.annotation.tailrec

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

  // Tail Recursive Mandelbrot Computation
  def compute(cloc:Complex):Int = {
    compute(cloc, Complex.zero, 0, topOut)
  }

  @tailrec
  def compute(cloc:Complex, zloc:Complex, iteration:Int, topOut:Int):Int = {
    if (zloc.magnitude>2 || iteration >= topOut) {
      iteration
    } else {
      compute(cloc, zloc*zloc+cloc, iteration+1, topOut)
    }
  }

  def main(args: Array[String]) {

    val grad = new GradientMaker(topOut, RGB(255,0,0), RGB(0,0,255))

    val ppm = new PPMWriter("output.ppm", pixelWidth, pixelHeight)

    (0 until pixelHeight).foreach( row => {

      print('.')

      val imgLoc  = top + (row * hpixelDelta)

      ppm.writePixel(
        grad.mapIterations(
          (0 until pixelWidth).map( col => {
            val realLoc = left + (col * wpixelDelta)
            compute(Complex(realLoc, imgLoc))
          }).toList
        )
      )

    })

    ppm.done()
  }

}
