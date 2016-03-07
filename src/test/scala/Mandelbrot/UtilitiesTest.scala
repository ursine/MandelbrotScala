package Mandelbrot

import org.scalatest._

class UtilitiesTest extends FlatSpec with Matchers {
  "a minimum integer" should "be less than a maximum" in {
    Utilities.min(4,5) should be (4)
    Utilities.min(-4,5) should be (-4)
    Utilities.min(400,5) should be (5)
    Utilities.min(0.1,0.2) should be (0.1)
  }

  "a maximum integer" should "be greater than a minimum" in {
    Utilities.max(4,5) should be (5)
    Utilities.max(-4,5) should be (5)
    Utilities.max(400,5) should be (400)
    Utilities.max(0.1,0.2) should be (0.2)
  }
}
