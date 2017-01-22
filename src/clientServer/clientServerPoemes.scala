package clientServer

/*
 *Auteur: Keil Laurent
 *Date: 19 mars 2015 
 *Description: client-server poèmes
 */
import java.net._
import java.io._
import scala.io._
import Stream._
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global 
import scala.io.Source
import scala.util.Random
import scala.util.{Try, Success, Failure} 
import rx.lang.scala.{Observable, Subscription} 
import rx.lang.scala.schedulers.NewThreadScheduler
import java.util.HashSet
  
/*  
 * Création de la classe abstraite Phone   
 * Création de Case class : pour les phones avec Voyelle et Consonne qui étendent Phone (+héritage)
 */
abstract class Phone
case class Phones(phs : Phone, ph : Phone) extends Phone
case class Voyelle(v : Char) extends Phone
case class Consonne(c : Char) extends Phone 

/* 
 * Classe des mots
 */
class Mot(mot:String, nbSyl:Int, phone:String) {
    
    def m = mot //chaine du mot
    def nb = nbSyl //nombre de syllabes du mot
    def ph = phone //phonétique du mot
    
    /* -utilisation de variables non-mutables
     * -structure de donnée non-mutable en scala (Set)
     * l'ensemble des voyelles possibles en phone
     */
    private val voyelles = Set("a","e","i","o","u","y","à","è","ù","é","â","ê","î","ô","û","ä","ë","ï","ö","ü","E","§","2","5","9","8","£","@")
  
    /* Surcharge de toString pour un mot, j'ai décidé de ne pas y indiquer que la simple information du mot en tant que string 
     * mais d'y ajouter également le nombre de syllabe et sa phonétique
     */
    override def toString() : String = m + ", " + nb + " syllabes, phone = '" + ph + "'"
    
    //return le dernier caractere de str
    def lastC (str : String) : Char = str.charAt(str.length()-1)
    /* 
     * méthode de haut niveau contains sur Set voyelles (structure de donnée non-mutable)
     * return true si v est une voyelle
     */
    def isVoy (v : Char) : Boolean = voyelles.contains(v.toString())
    /*
     * pattern matching sur case class
     * return le dernier phone de phone
     */
    def lastPh (phone:Phone) : Char = phone match {
          case Phones(phs, Voyelle(ph)) => ph
          case Phones(phs, Consonne(ph)) => ph
          case Voyelle(ph) => ph
          case Consonne(ph) => ph
    }
    /*
     * pattern matching sur case class
     * return les premiers phones de phone (tous sauf le dernier)
     */
    def bodyPh (phone:Phone) : Phone = phone match {
          case Phones(phs, Voyelle(ph)) => phs
          case Phones(phs, Consonne(ph)) => phs
    }
    /*
     *pattern matching sur Int + récursivité
     * return le phone correspondant à la chaine de caractère ph
     */
    def toPhone (ph : String) : Phone = ph.length() match {
          case 1 => if(isVoy(lastC(ph))) 
                        Voyelle(lastC(ph)) 
                    else
                        Consonne(lastC(ph))
          case _ => if(isVoy(lastC(ph))) 
                        Phones( toPhone(ph.substring(0, ph.length()-1)), Voyelle(lastC(ph)) ) 
                    else   
                        Phones( toPhone(ph.substring(0, ph.length()-1)), Consonne(lastC(ph)) )
    }
    
    /*
     * Pattern matching sur des case class (Phone) + récursivité
     * return true si phone et autre_phone ont la même phonétique (voir fonction rime_avec)
     */
   def rime (phone:Phone, autre_phone:Phone) : Boolean = autre_phone match {
          case Phones(phs, Voyelle(ph)) => ph == lastPh(phone)
          case Phones(phs, Consonne(ph)) => ph == lastPh(phone) && rime(phs, bodyPh(phone))
          case _ => false //pas de mot identique, donc si on se réduit à un seul phone, les 2 mots ne riment d'office pas.
    }
   /*
    * return true si 2 mots m1 et m2 riment cad ssi :
     *   le dernier phone (son) de m1 et le dernier phone de m2 sont des voyelles identiques
     *   OU
     *   le dernier phone de m1 et le dernier phone de m2 sont des consonnes identiques ET les deux mots, amputés de ces deux consonnent, riment
    */
   def rime_avec (autre_mot : Mot) : Boolean = rime(toPhone(this.ph), toPhone(autre_mot.ph))
   
}

/*
 * Classe des phrases
 */
class Phrase (phrase : String, mots_hachage : Map[String,Mot]) {
    /*
     * Un token est un groupe de lettre séparé par des signes de ponctuation
     * (notamment des espaces).  C'est ce qu'on appelle généralement des "mots".
     */
    private val tokens = Phrases.split_mots(phrase.toLowerCase)
    
    /* La liste des mots de la phrase, return un Array[Mot] des Mots de la phrases */    
    val mots = for {
                    t<-tokens
               } yield mots_hachage(t)
      
    //Surcharche de méthode toString pour écrire une phrase
    override def toString() : String = phrase 
    
    /* 
     * -utilisation de méthodes de haut niveau (map et sum)
     * -utilisation de fonction anonyme { x => x.nb }
     * -application partielle
     *
     * Détermine le nombre de syllabes de la phrase en sommant chaque nombre de syllabes des mots de la phrase.
     * Remarque : Le nombre de syllabes peut être discuté... Il n'est pas toujours correct dans "dicorimes"
     */
    val syllabes : Int = mots.map { x => x.nb }.sum
    
    /*Deux phrases riment si le dernier mot de l'une rime avec le dernier mot de l'autre.*/
    def rime_avec (autre_phrase : Phrase) : Boolean = this.mots(mots.length-1).rime_avec(autre_phrase.mots(autre_phrase.mots.length-1))
}

/*Cet objet compagnon permet de créer une phrase sans utiliser new Phrase(...) mais en mettant directement Phrase(...)*/
object Phrase {
    def apply (phrase:String, mots_hachage:Map[String,Mot]) = new Phrase (phrase, mots_hachage)
}

/*
 * Objet pour l'extraction des phrases du texte et relation avec un dictionnaire contenant les mots de ce texte.
 */
object Phrases {
    def split_mots (s:String) : Array[String] =  s.trim.toLowerCase.split("[- ’—,;'()\"!.:?]+")
    def split_phrases (s:String) : Array[String] = s.split("(?<=[.!?:])")
    def lire_csv (chemin:String, mots:Set[String]) : Try[List[String]] =  Try({  
                                                                                (for {
                                                                                      line <- Source.fromFile(chemin).getLines()  
                                                                                      if mots contains line.split(",")(1)
                                                                                     } yield line
                                                                                ).toList 
                                                                           })
                                                                   
    def extraire_txt(chemin_texte : String) : Try[Stream[String]] = {
        /*
          Liste des phrases du texte
        */
       for {
            texte <- Try(Source.fromFile(chemin_texte).getLines().filter(_!="").foldLeft(""){_+_})
            phrases_txt = split_phrases(texte)
       } yield phrases_txt.toStream
    }
   
    def extraire_phrases(chemin_dico : String, texte : List[String]) : Try[List[Phrase]] = {
       
       /*
          Liste des phrases du texte dont tous les mots font partie du dictionnaire 
          et pas trop grandes ni trop petites
        */
       for {
            mots_set <- Try(split_mots(texte mkString).toSet)
            dico <- lire_csv(chemin_dico, mots_set)
            mots_hachage = dico.map { line => ( (line.split(",")(1)), new Mot(line.split(",")(1), line.split(",")(6).toInt, line.split(",")(8)) )  }.toMap
            phrases <- Try(texte filter (p => (((split_mots(p) map (mots_hachage contains _)) forall (x=>x)) && p.trim!="")) map (p => Phrase(p.trim, mots_hachage)))
       } yield phrases filter (p => p.syllabes > 5 && p.syllabes < 15)
           
    }
}

/*trait generator pour génération d'un type*/
trait Generator[T] {
        self =>
        def generate : T
        def map[S](f: T => S): Generator[S] = new Generator[S] {
          def generate = f(self.generate)
        }
        def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
          def generate = f(self.generate).generate
        }
        def filter(p: T => Boolean): Generator[T] = new Generator[T] {
          val gen = self.generate
          def generate = {
             if(p(gen)) gen else this.generate
          }
        }
}
    
/*
 * Classe abstraite pour la création d'un poème
 */
abstract class Poeme(phrases:List[Phrase]) {
   
    //Générateur d'entiers
    val ints = new Generator[Int] {
        val rand = new java.util.Random
        def generate = rand.nextInt()
    }
    
    //Generateur aleatoire de phrases
    private val phrases_aleatoires = for (x <- ints) yield phrases(x.abs % phrases.length)
    
    //fonction pour savoir s'il existe au moins un couple de phrases qui riment avec quasi même nombre de syllabes
    def existCoupleRime = (for {
                            p1 <- phrases
                            p2 <- phrases
                            exist = ((p1!=p2) &&  (p1.syllabes - 2 <= p2.syllabes && p1.syllabes + 2 >= p2.syllabes) && (p1 rime_avec p2))
                          } yield exist)  
                          
    /* 
     * Generation de couples de phrases qui riment
     * + transformation de for comprehension en flatmap, filter et map
     */
    val couple_riment : Option[List[(Phrase,Phrase)]] = {
        if (!(existCoupleRime.contains(true))){
          None
        }
        else {
          Some ( 
              Random.shuffle(
                    phrases flatMap (p1 => 
                                      phrases filter (p2 => ((p1!=p2) && (p1.syllabes - 2 <= p2.syllabes && p1.syllabes + 2 >= p2.syllabes) && (p1 rime_avec p2))) 
                                              map (p2 => (p1,p2))
                                    )
                )
             
          )
        }
    }

    /*Renvoie un poème*/
    def ecrire() : List[Phrase]
}

/*
 * Classe permettant de créer un poème de deux vers
 * héritage de la classe abstraite Poeme
 */
class DeuxVers(phrases:List[Phrase]) extends Poeme(phrases:List[Phrase]) {
    
   /*
    * Écrire un petit poème de 4 vers seulement s'il existe au moins un couple de phrases qui riment et ont presque le même nombre de syllabes,
    * renvoie une erreur sinon.
    * -utilisation de méthode de haut niveau sur liste (map)
    * -utilisation de structure de donnée (List)
    * -utilisation de fonction anonyme (pour découpler et afficher proprement les vers)
    */
   def ecrire() : List[Phrase] = { //recherche de couples de phrases qui riment en parallèle, exception si aucun couple trouvé.
           couple_riment match {
                case Some(list) => {
                    val coupleGen = list.distinct.toSet; //en set pour ne pas avoir de doublons !
                    if (coupleGen.head == coupleGen.last) throw new Exception("Il n'existe pas de couple de phrases qui riment et qui ont presque le même nombre de syllabes.")
                    List(coupleGen.head._1, coupleGen.head._2)
                }
                case None => throw new Exception("Il n'existe pas de couple de phrases qui riment et qui ont presque le même nombre de syllabes.")
           } 
   }
}

/*
 * Client permettant d'extraire et d'envoyer sur le serveur les phrases une par une
 */
object Client{
    def main(args:Array[String]):Unit={    
        val client = new Client(InetAddress.getLocalHost,9999, phrases )
        client.start()
    }
    def phrases():Stream[String] = {
      
        val chemin_corpus:String = "maupassant.txt" //texte contenant des phrases utilisé pour sélectionner des vers qui riment    
        /*Extraction du texte du corpus en y ajoutant une fin, exception si corpus non-trouvé*/
        val texte = Phrases.extraire_txt(chemin_corpus) match { 
            case Success(txt) => txt ++ Stream("Fin du corpus de phrases")
            case Failure(exc) => Stream("Le corpus de phrases est introuvable")
        }
        
        /*Envoi de stream contenant chacun une phrase au serveur*/
        def phrases_aux(n:Int):Stream[String] = {
            texte(n)#::phrases_aux(n+1)
        }
        phrases_aux(0)
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

/* 
 * Serveur réceptionnant les phrases et générant des poèmes de quatrain avec celles-ci au fur et à mesure de la réception 
 */
object Server{
  
    def main(args:Array[String]):Unit= {
        
        val chemin_dico:String = "dicorimes.dmp" //dictionnaire contenant une liste de mots associés à leur nombre de syllabes et à leur phonétique.     
        val maxPhrases = 200 //nombre de phrases pour générer un quatrain
        val maxPoemes = 5 //nombre de poèmes à générer
        val server = new ServerSocket(9999)
        
        var nb = 0
        var nbPoemes = 0
        var listPhrase = List("") //liste de phrases pour générer un poème
        
        val end = Promise[Boolean] //promise pour arrêter le programme.
        val arret = end.future
        val poemes = Promise[List[String]]
        val listPhrases = poemes.future
        
        val obs = Observable.from(receive(poemes,server)) //récupère le texte au fur et à mesure
        val obsFilt = obs.filter(x => x.length() > 20 && x.length < 60) //filtre plus joli
        val subscriptionObserver = obsFilt
                  .subscribeOn(NewThreadScheduler())
                  .subscribe({ 
                            x => 
                            if (x == "Fin du corpus de phrases" || x == "Le corpus de phrases est introuvable") {
                                 println(x) //Exception
                                 end success true
                            }
                            nb += 1
                            listPhrase = x::listPhrase
                            if(nb == maxPhrases) {
                                  val doublet1 = for {
                                      //extraction des phrases du texte utilisée pour générer le poème
                                      phrs <- Phrases.extraire_phrases(chemin_dico, listPhrase.take(listPhrase.length/2))
                                  } yield new DeuxVers(phrs).ecrire
                                  val doublet2 = for {
                                      //extraction des phrases du texte utilisée pour générer le poème
                                      phrs <- Phrases.extraire_phrases(chemin_dico, listPhrase.takeRight(listPhrase.length/2))
                                  } yield new DeuxVers(phrs).ecrire
                                  //génération du poème de 4 vers aléatoire et affichage du poeme si pas d'erreur, sinon affichage de l'erreur
                                  doublet1 match {
                                      case Success(vers1) => { 
                                               doublet2 match {
                                                     case Success(vers2) =>  println (vers1(0).toString + "\n" + vers2(0).toString + "\n" 
                                                                       + vers2(1).toString + "\n" + vers1(1).toString + "\n")
                                                     case Failure(ex) => if(ex.getMessage != "Il n'existe pas de couple de phrases qui riment et qui ont presque le même nombre de syllabes."){
                                                                              println("Le dictionnaire de rimes est introuvable")
                                                                              end success true
                                                                         } else {
                                                                              println(ex.getMessage + "\n")
                                                                         }
                                                              }
                                                             }
                                      case Failure(ex) => if(ex.getMessage != "Il n'existe pas de couple de phrases qui riment et qui ont presque le même nombre de syllabes."){
                                                              println("Le dictionnaire de rimes est introuvable")
                                                              end success true
                                                          } else {
                                                              println(ex.getMessage + "\n")
                                                          }
                                  }
                                 nbPoemes+=1
                                 nb = 0
                                 listPhrase = List("")
                                 if(nbPoemes == maxPoemes) end success true
                            }  
                  })
        
        Await.ready(arret, Duration.Inf) 
        print("\n                         Par Keil Laurent\n                         19/03/2015")
        
        subscriptionObserver.unsubscribe()
    }
    
    def receive(poemes:Promise[List[String]],server:ServerSocket):Stream[String] = {
        val s = server.accept()
        val in = new BufferedSource(s.getInputStream()).getLines()
        if(in.hasNext){
            in.next()#::receive(poemes,server)
        }
        else {
            s.close()
            println("arret")
            poemes success List()
            empty
        }
    }
}

/*
 * Objet et méthode principaux permettant de lancer le programme de génération automatique de quatrain en client-server
 *    OBJECTIFS : 
 * Gestion Client-Server, Observable, Stream, suscribe et toutes les fonctions d'observable et client-server
 * Filtre en amont des phrases pour plus joli et plus optimisé sur la recherche par-après
 * Gestion des exceptions : mauvais corpus, dictionnaire, si pas assez de phrases pour générer un poèmes (continue à essayer de générer sur le reste du texte), doublons (dans une même séquence), fin du corpus...
 * Configuration du nombre de phrases à envoyer pour générer chaque quatrain ET nombre de poèmes à générer
 * Fonctionne sur tout corpus (attendre un peu pour gros corpus...)
 */
object Main {
  
    //programme principal
    def main(args:Array[String]) {
      
        println("Poème :")
        println("_______\n")
        
        val server = Future { Server.main(Array()) }
        val client = Future { Client.main(Array()) }
        
        Thread.sleep(50000)
        
    } 
}