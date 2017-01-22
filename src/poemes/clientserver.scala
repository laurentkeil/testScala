package poemes

/**
 * @author laurent
 */
import java.net._
import java.io._
import scala.io._
import Stream._
import rx.lang.scala.{Observable, Subscription} 
import rx.lang.scala.schedulers.NewThreadScheduler
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object Client{
    def main(args:Array[String]):Unit={
        val client = new Client(InetAddress.getLocalHost,9994,(naturels map (_.toString)))
        client.start()
    }
    def naturels():Stream[Int] = {
        def naturels_aux(n:Int):Stream[Int] = {
            Thread.sleep(1000)
            n#::naturels_aux(n+1)
        }
        naturels_aux(0)
    }
}
class Client(adresse:InetAddress,port:Int,stream:Stream[String]){
    def start():Unit = {
        val donnees = stream.iterator
            while(donnees.hasNext){
                val s = new Socket(adresse,port)
                val out = new PrintStream(s.getOutputStream())
                out.println(donnees.next)
                out.flush()
                s.close()
            }
    }
}

object Server{
    def main(args:Array[String]):Unit= {
        var nb = 0
        val server = new ServerSocket(9994)
        val end = Promise[List[String]]
        val listPhrases = end.future
        var listPhrase = List("")
        val obs = Observable.from(receive(end,server))
        val subscriptionObserver = obs
                                      .subscribeOn(NewThreadScheduler())
                                      .subscribe({x=> nb+=1;listPhrase = x::listPhrase;println(x);if(nb>9) end success listPhrase.reverse })
        listPhrases onSuccess {
          case x => println(x)
        }
        Await.ready(end.future,Duration.Inf)
        subscriptionObserver.unsubscribe()
    }
    def receive(end:Promise[List[String]],server:ServerSocket):Stream[String] = {
        val s = server.accept()
        val in = new BufferedSource(s.getInputStream()).getLines()
        if(in.hasNext){
            in.next()#::receive(end,server)
        }
        else {
            s.close()
            println("arret")
            end success List("ok")
            empty
        }
    }
}

object Test {
    def main(args:Array[String]) = {
      /*def nats2(n:Long):Stream[Long] = {
        Thread.sleep(500)
        n #:: nats2(n+1)
      }
      val obs: Observable[Long] = Observable.from(nats2(0))
      val primes = obs.filter(x=> (x%2) == 0)
      val sub = primes
                    .subscribeOn(NewThreadScheduler())
                    .subscribe(println(_)) 
      readLine()
      sub.unsubscribe()*/
      
      val server = Future { Server.main(Array()) }
      val client = Future { Client.main(Array()) }
      
      Thread.sleep(15000)
      
    }
}