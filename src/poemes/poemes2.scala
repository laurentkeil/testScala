/*
 *Auteur: Keil Laurent
 *Date: 5 mars 2015
 *Description: poèmes de promesses futures
 */
package poemes

import scala.io.Source
import scala.util.Random
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
 
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
    def lire_csv (chemin:String, mots:Set[String]) : List[String] =       {  
                                                                                (for {
                                                                                      line <- Source.fromFile(chemin).getLines()  
                                                                                      if mots contains line.split(",")(1)
                                                                                     } yield line
                                                                                ).toList 
                                                                          }
                                                                   
    def extraire_phrases(chemin_texte : String, chemin_dico : String) : Future[List[Phrase]] = {
      
        /*
          Liste des phrases du texte dont tous les mots font partie du dictionnaire
          Future permmettant l'exctraction de phrases dans des corpus différents en parallèle
        */
       Future { for {
            texte <- List(Source.fromFile(chemin_texte).getLines().filter(_!="").foldLeft(""){_+_})
            phrases_txt = split_phrases(texte)
            mots_set = split_mots(texte).toSet
            dico = lire_csv(chemin_dico, mots_set)
            mots_hachage = dico.map { line => ( (line.split(",")(1)), new Mot(line.split(",")(1), line.split(",")(6).toInt, line.split(",")(8)) )  }.toMap
            phrases <- (phrases_txt filter (p => (((split_mots(p) map (mots_hachage contains _)) forall (x=>x)) && p.trim!="")) map (p => Phrase(p.trim, mots_hachage)))
       } yield phrases }
           
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
    
    //fonction qui génère des phrases qui riment avec p1 (avec un nombre de syllabes de différence max de 1)
    def list_phr_riment (p1 : Phrase) = for {
                            p2 <- phrases
                            if ((p1!=p2) && (p1.syllabes > 5 && p2.syllabes > 5 && p1.syllabes < 15 && p1.syllabes - 2 <= p2.syllabes && p1.syllabes + 2 >= p2.syllabes) && (p1 rime_avec p2))
                          } yield p2
    def existCoupleRime (p1 : Phrase) = (for {
                            p2 <- phrases
                            exist = ((p1!=p2) && (p1.syllabes > 5 && p2.syllabes > 5 && p1.syllabes < 15 && p1.syllabes - 2 <= p2.syllabes && p1.syllabes + 2 >= p2.syllabes) && (p1 rime_avec p2))
                          } yield exist) 
    
    //fonction pour savoir s'il existe au moins un couple de phrases qui riment avec quasi même nombre de syllabes
    def existCoupleRime = (for {
                            p1 <- phrases
                            p2 <- phrases
                            exist = ((p1!=p2) && (p1.syllabes > 5 && p2.syllabes > 5 && p1.syllabes < 15 && p1.syllabes - 2 <= p2.syllabes && p1.syllabes + 2 >= p2.syllabes) && (p1 rime_avec p2))
                          } yield exist)  
                          
    /* 
     * Generateur aleatoire de couples de phrases qui riment
     * + transformation de for comprehension en flatmap, filter et map
     */
    val couple_riment : Option[Generator[List[(Phrase,Phrase)]]] = {
        if (!(existCoupleRime.contains(true))){
          None
        }
        else {
          Some ( 
             new Generator[List[(Phrase,Phrase)]] { 
                def generate = Random.shuffle(
                    phrases flatMap (p1 =>  //filtre sur des phrases avec plus de 5 syllabes et une différence maximum de 2
                                      phrases filter (p2 => ((p1!=p2) && (p1.syllabes > 5 && p2.syllabes > 5 && p1.syllabes < 15 && p1.syllabes - 2 <= p2.syllabes && p1.syllabes + 2 >= p2.syllabes) && (p1 rime_avec p2))) 
                                              map (p2 => (p1,p2))
                                    )
                )
             } 
          )
        }
    }

    /*Renvoie un poème*/
    def ecrire() : Future[List[Phrase]] 
}

/*
 * Classe permettant de créer un poème de deux vers
 * héritage de la classe abstraite Poeme
 */
class DeuxVers(phrases:List[Phrase]) extends Poeme(phrases:List[Phrase]) {
  
   /*
    * Écrire un petit poème de deux vers seulement s'il existe au moins un couple de phrases qui riment et ont presque le même nombre de syllabes,
    * renvoie une erreur sinon.
    * -utilisation de méthode de haut niveau sur liste (map)
    * -utilisation de structure de donnée (List)
    * -utilisation de fonction anonyme (pour découpler et afficher proprement les vers)
    */
   def ecrire() : Future[List[Phrase]] = Future { //Future permettant la recherche de couple de phrases qui riment en parallèle, exception si aucun couple trouvé.
           couple_riment match {
                case Some(x) => val coupleGen = x.generate.head ; List(coupleGen._1, coupleGen._2)
                case None => throw new Exception("Il n'existe pas de couple de phrases qui riment et qui ont presque le même nombre de syllabes.")
           } 
   }
}
 
/*
 * Objet et méthode principaux permettant de lancer le programme de génération automatique de poèmes de 2 vers
 */
object Main {
  
    //complète un doublet si un couple de phrases a été trouvé, complète la quatrain directement avec une exception sinon
    def completer (lines : List[Phrase], doublet : Promise[List[Phrase]], quatrain : Promise[String]) : Unit = {
            new DeuxVers(lines).ecrire onComplete {
                  case Success(vers) => doublet success vers
                  case Failure(ex) => quatrain failure ex
            }
    }
    
    /*
      si l'extration de texte est terminée et réussie, complète un doublet si un couple de phrases a été trouvé ou complète la quatrain directement avec une exception sinon ;
      si elle a échouée, extrait à partir d'un autre texte de secours et d'un dictonnaire de secours, permettant de faire la même opération ;
      si l'extraction échoue une nouvelle, complète directement le quatrain avec une exception 
    */
    def doublet (texte : Future[List[Phrase]], doublet : Promise[List[Phrase]], quatrain : Promise[String], chemin_corpus_secours:String, chemin_dictionnaire_secours:String) : Unit = {
            texte onComplete {
                  case Success(lines) => completer(lines, doublet, quatrain)
                  case Failure(ex) =>
                      val texte_secours = Phrases.extraire_phrases(chemin_corpus_secours,chemin_dictionnaire_secours)
                      texte_secours onComplete {
                        case Success(lines) => completer(lines, doublet, quatrain)
                        case Failure(ex) => quatrain failure ex //"Erreur dans l'extraction de fichiers."  
                      }
            }
    }
  
    //programme principal
    def main(args:Array[String]) {
        val chemin_corpus_1:String = "corpus.txt" //texte contenant des phrases utilisé pour sélectionner des vers qui riment
        val chemin_corpus_2:String = "dixcontes.txt"
        val chemin_corpus_1_secours:String = "daudet.txt" //les dictionnaires de secours mettent plus de temps mais ça fonctionne
        val chemin_corpus_2_secours:String = "zola.txt"
        val chemin_dictionnaire_1:String = "dicorimes.dmp" //dictionnaire contenant une liste de mots associés à leur nombre de syllabes et à leur phonétique.
        val chemin_dictionnaire_secours:String = "dicorimes.dmp" //dictionnaire de secours
        
        val quatrain = Promise[String] //quatrain promis
        val q = quatrain.future
        
        val doublet1 = Promise[List[Phrase]]
        val d1 = doublet1.future
        val doublet2 = Promise[List[Phrase]]
        val d2 = doublet2.future
        
        Future {
            val texte1 = Phrases.extraire_phrases(chemin_corpus_1,chemin_dictionnaire_1)
            doublet(texte1, doublet1, quatrain, chemin_corpus_1_secours, chemin_dictionnaire_secours)
          
            val texte2 = Phrases.extraire_phrases(chemin_corpus_2,chemin_dictionnaire_1)
            doublet(texte2, doublet2, quatrain, chemin_corpus_2_secours, chemin_dictionnaire_secours)
        }
        
        Future {
            //met en ordre de rimes embrassées lorsque les 2 doublets ont été trouvés.
            d1 onSuccess {
                case versDoublet1 =>         
                d2 onSuccess {
                      case versDoublet2 => quatrain success versDoublet1(0).toString + "\n" + versDoublet2(0).toString + "\n" + versDoublet2(1).toString + "\n" + versDoublet1(1).toString
                }
            }  
            Await.ready(d1, Duration.Inf)
            Await.ready(d2, Duration.Inf)    
            q onComplete {
                case Success(poeme) => println(poeme) //on imprime le quatrain mis en ordre de rimes embrassées si pas d'exception
                case Failure(ex) => println(ex)  //on imprime l'exception sinon
            }
        }
        
        Await.ready(q, Duration.Inf) 
        Thread sleep 30000
        
    } 
}