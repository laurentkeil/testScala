import java.util.{Date, Locale}   //import 2
//import java.text.DateFormat
import java.text.DateFormat._     //_ = * pour import tout de DateFormat

object FrenchDate {
  def main(args : Array [String]) {
      val now = new Date(2015,2,17)                               //current date 
      val df = getDateInstance(LONG, Locale.FRANCE)    //format
      println (df format now)                          //= df.format(now)
  } 
}