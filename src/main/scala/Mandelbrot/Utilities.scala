package Mandelbrot

object Utilities {

  def min[A](in1:A, in2:A)(implicit e: A => Ordered[A]): A = if (in1<in2) in1 else in2
  def max[A](in1:A, in2:A)(implicit e: A => Ordered[A]): A = if (in1>in2) in1 else in2

}
