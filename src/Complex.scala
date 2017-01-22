
//Classe Scala avec args
class Complex (real:Double, imaginary: Double) { //hérite implicitement de la super-classe AnyRef
  def re = real      //sans argument et sans () - re() = real est possible
  def im = imaginary
  var age = 20
  def upAge = age = age + 1
  override def toString() =   //surcharge de méthode toString
      "" + re + (if (im < 0 ) " " else " + ") + im + "i"
}

object ComplexNumbers {
  def main (args : Array[String]) {
    val c = new Complex(1.2, 3.4)        //instancie la classe Complex
    println("imaginary part : " + c.im)  //appel méthode im
    println(c.toString())
    println("\nage : " + c.age)
    c.upAge
    println("age : " + c.age)
  }
}