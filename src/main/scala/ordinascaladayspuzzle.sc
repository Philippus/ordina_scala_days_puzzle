object OrdinaScalaDaysPuzzle {
  def addOneUntilMaxIs99(array: Array[Int]): Array[Int] = {
    if (array.max != 99) addOneUntilMaxIs99(array.map(b => b + 1))
    else array
  }

  val lettersToCounterparts = ('a' to 'z').zip(('a' to 'z').reverse)

  val phrase = "rats live on arona no evil star"

  val step1 = phrase.map(c => lettersToCounterparts.find(d => d._1 == c) match {
    case Some((k, v)) => v
    case None => c
  })

  val step2 = step1.getBytes().map(b => b - 'a'.toByte + 1)

  val step3 = addOneUntilMaxIs99(step2)
  val step4 = step3.reverse
  val step5 = step4.map(b => 100 - b)
  val step6 = step5.map(b => b + 'a'.toByte - 1).map(b => b.toChar).map(c => if (c < 'a' || c > 'z') " " else c).mkString

  val allStepsAtOnce = addOneUntilMaxIs99(phrase.map(c => lettersToCounterparts.find(d => d._1 == c) match {
    case Some((k, v)) => v
    case None => c
  }).getBytes().map(c => c - 'a'.toByte + 1)).reverse.map(b => 100 - b).map(c => c + 'a'.toByte - 1).map(b => b.toChar).map(c => if (c < 'a' || c > 'z') " " else c).mkString

}