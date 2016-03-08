package Mandelbrot

import java.awt.Color
import java.awt.image.BufferedImage

import scala.annotation.tailrec
import scala.swing._

object Mandelbrot extends SimpleSwingApplication {

  private val leftMand:Double   = -2
  private val topMand:Double    = -1
  private val rightMand:Double  = 2
  private val bottomMand:Double = 1
  private val pixelWidth    = 800
  private val pixelHeight   = 400

  private val width:Double  = Math.abs(leftMand - rightMand)
  private val height:Double = Math.abs(topMand - bottomMand)
  private val wpixelDelta:Double = width/pixelWidth
  private val hpixelDelta:Double = height/pixelHeight

  private val topOut = 10000

  private val img:BufferedImage = new BufferedImage(pixelWidth, pixelHeight, BufferedImage.TYPE_3BYTE_BGR)

  private val lock = new Object

  val grad = new GradientMaker(topOut, RGB(255,0,0), RGB(0,0,255))

  lazy val ui = new Panel {
    background = Color.white
    preferredSize = new Dimension(pixelWidth, pixelHeight)
    focusable = true

    var drawThread = new Thread(new Runnable {
      def run = {
        (0 until pixelHeight).foreach(row => {
          val imgLoc = topMand + (row * hpixelDelta)
          val data: List[RGB] = grad.mapIterations(
            (0 until pixelWidth).map(col => {
              val realLoc = leftMand + (col * wpixelDelta)
              compute(Complex(realLoc, imgLoc))
            }).toList)

          data.zipWithIndex.foreach( index => {
            val (clr, xpix) = index
            img.setRGB(xpix,row,new Color(clr.red,clr.green,clr.blue).getRGB)
          })
          repaint
          revalidate
        })
      }
    })

    drawThread.start()

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawImage(img, 0, 0, null)
    }
  }

  // Tail Recursive Mandelbrot Computation
  def compute(cloc:Complex):Int = {

    @tailrec
    def compute(cloc:Complex, zloc:Complex, iteration:Int, topOut:Int):Int = {
      if (zloc.magnitude>2 || iteration >= topOut) {
        iteration
      } else {
        compute(cloc, zloc * zloc + cloc, iteration + 1, topOut)
      }
    }

    compute(cloc, Complex.zero, 0, topOut)
  }

  def top = new MainFrame {
    title = "Mandelbrot Drawing"
    contents = ui
  }


  /*def main(args: Array[String]) {



    val ppm = new PPMWriter("output.ppm", pixelWidth, pixelHeight)



    })

    ppm.done()
  }*/

}
