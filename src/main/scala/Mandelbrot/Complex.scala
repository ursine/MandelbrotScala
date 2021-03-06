package Mandelbrot

import scala.math.sqrt

class Complex(val re:Double, val im:Double) {

  def +(c: Complex):Complex = Complex(re + c.re, im + c.im)
  def *(c: Complex):Complex = Complex(re * c.re - im * c.im, im * c.re + re * c.im)

  def magnitude = sqrt(re*re + im*im)

  override def toString =
    "%s%s*i".format(re, if (im < 0) "-" + -im else "+" + im)
}

object Complex {

  def zero = Complex(0,0)

  def apply(re:Double, im:Double):Complex = new Complex(re,im)
}