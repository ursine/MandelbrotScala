package Mandelbrot

import java.awt.Color
import java.awt.image.BufferedImage

import scala.annotation.tailrec
import scala.swing._
import scala.swing.event._

object Mandelbrot extends SimpleSwingApplication {

  private var leftMand:Double   = -2
  private var topMand:Double    = -1
  private var rightMand:Double  = 2
  private var bottomMand:Double = 1

  private val pixelWidth    = 800
  private val pixelHeight   = 400

  private var width:Double  = 0
  private var height:Double = 0
  private var wpixelDelta:Double = 0
  private var hpixelDelta:Double = 0

  def updateMetadata() = {
    width = Math.abs(leftMand - rightMand)
    height = Math.abs(topMand - bottomMand)
    wpixelDelta = width / pixelWidth
    hpixelDelta = height / pixelHeight
  }
  updateMetadata()

  private val topOut = 1000

  private var img:BufferedImage = new BufferedImage(pixelWidth, pixelHeight, BufferedImage.TYPE_3BYTE_BGR)

  val grad = new GradientMaker(topOut, RGB(255,0,0), RGB(0,0,255))

  var xclick:Double = 0
  var yclick:Double = 0

  lazy val ui = new BorderPanel {
    background = Color.white
    preferredSize = new Dimension(pixelWidth, pixelHeight)
    focusable = true

    listenTo(mouse.clicks, mouse.moves)

    reactions += {
      case e: UIElementResized => println("Panel RESIZED")

      case e: MousePressed => {
        xclick = e.point.getX
        yclick = e.point.getY
        println(s"Mouse Pressed $xclick, $yclick")
      }

      case e: MouseReleased => {
        val xend = e.point.getX
        val yend = e.point.getY

        val lMand = leftMand + (xclick * hpixelDelta)
        val rMand = leftMand + (xend * hpixelDelta)
        val tMand = topMand + (yclick * wpixelDelta)
        val bMand = topMand + (yend * wpixelDelta)

        println(s"Mouse Released $xend, $yend")
        println(s"New Location: ($lMand,$tMand) - ($rMand,$bMand) vs ($leftMand,$topMand) - ($rightMand,$bottomMand)")

        leftMand = lMand
        rightMand = rMand
        topMand = tMand
        bottomMand = bMand

        img = new BufferedImage(pixelWidth, pixelHeight, BufferedImage.TYPE_3BYTE_BGR)

        updateMetadata()
        drawThread = makeDrawer()
      }

      case _: FocusLost => repaint()
    }

    def makeDrawer() = {
      val t = new Thread(new Runnable {
        def run = {
          (0 until pixelHeight).foreach(row => {
            val imgLoc = topMand + (row * hpixelDelta)
            val data: List[RGB] = grad.mapIterations(
              (0 until pixelWidth).par.map(col => {
                val realLoc = leftMand + (col * wpixelDelta)
                compute(Complex(realLoc, imgLoc))
              }).toList)

            data.zipWithIndex.foreach(index => {
              val (clr, xpix) = index
              img.setRGB(xpix, row, new Color(clr.red, clr.green, clr.blue).getRGB)
            })
            repaint
            revalidate
            print('.')
          })
        }
      })
      t.start()
      t
    }
    var drawThread = makeDrawer()

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

    reactions += {
      case e: UIElementResized => println("Main Frame RESIZED")
    }
  }


  /*def main(args: Array[String]) {



    val ppm = new PPMWriter("output.ppm", pixelWidth, pixelHeight)



    })

    ppm.done()
  }*/

}
