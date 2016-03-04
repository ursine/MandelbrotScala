case class GradientMaker(steps:Int, color1:RGB, color2:RGB) {

  private val amountPerStep:Double = 1.0 / steps.toDouble

  val finalGradient:List[RGB] = {
    List.tabulate(steps)( step => {
      val c2Pct = step * amountPerStep
      val c1Pct = 1.0 - c2Pct
      RGB(
        (c1Pct * color1.red   + c2Pct * color2.red).toInt,
        (c1Pct * color1.green + c2Pct * color2.green).toInt,
        (c1Pct * color1.blue  + c2Pct * color2.blue).toInt
      )
    })
  }

  def getStep(step:Int):RGB = { finalGradient( if (step>=steps) steps-1 else step ) }
}
