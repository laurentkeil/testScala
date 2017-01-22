object Test {
  def main (args : Array[String]) {
      val expr = (1 until 10) map (i => i*i)
      println(expr)
      
      val expr2 = for{i <- 1 until 10 if i % 2 == 0} yield (i*i)
      println(expr2)
      
      val expr3 = (for {
            i <- 1 until 10
            j <- 1 until i
            if (i+j) % 2 == 0
          } yield (i,j)).toList
          
      val expr3Translated = ( (1 until 10) flatMap (i => for (j <- 1 until i ; if (i+j) % 2 == 0) yield (i,j)) ).toList
      
      println(expr3)
      println(expr3Translated)
  }
}