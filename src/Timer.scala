object Timer {
  def oncePerSecond(callback : () => Unit) { //Unit similar to void() - param = fonction sans retour et prend pas d'args
    for (i <- 1 to 5) { //boucle 5 fois en appellant la méthode en argument
      callback()
      Thread sleep 1000
    }
  }
  def timeFlies() {
    println ("time ...")
  }
  def main (args : Array[String]){
    //oncePerSecond(timeFlies)  //appel avec fonction en param
    oncePerSecond ( () =>       //appel avec fonction anonyme en param
        println("time anonymous ...") 
      )
  }
}