
/*
 *Auteur: Keil Laurent
 *Date: 19 février 2015
 *Description: générateur automatique de poèmes
 */

/* For comprehension :
 * Création de la classe abstraite Phone
 * Création de Case class : pour les phones avec Voyelle et Consonne qui étendent Phone (+héritage)
 */
import scala.io.Source
import scala.util.Random

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
    
    /* For comprehension : -utilisation de variables non-mutables
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
     * For comprehension : méthode de haut niveau contains sur Set voyelles (structure de donnée non-mutable)
     * return true si v est une voyelle
     */
    def isVoy (v : Char) : Boolean = voyelles.contains(v.toString())
    /*
     * For comprehension : pattern matching sur case class
     * return le dernier phone de phone
     */
    def lastPh (phone:Phone) : Char = phone match {
          case Phones(phs, Voyelle(ph)) => ph
          case Phones(phs, Consonne(ph)) => ph
          case Voyelle(ph) => ph
          case Consonne(ph) => ph
    }
    /*
     * For comprehension : pattern matching sur case class
     * return les premiers phones de phone (tous sauf le dernier)
     */
    def bodyPh (phone:Phone) : Phone = phone match {
          case Phones(phs, Voyelle(ph)) => phs
          case Phones(phs, Consonne(ph)) => phs
    }
    /*
     * For comprehension : pattern matching sur Int + récursivité
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
     * For comprehension : Pattern matching sur des case class (Phone) + récursivité
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
     * For comprehension : -utilisation de méthodes de haut niveau (map et sum)
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
    def lire_csv (chemin:String, mots:Set[String]) : List[String] = { (for  {
                                                                              line <- Source.fromFile(chemin).getLines()  
                                                                              if mots contains line.split(",")(1)
                                                                            } yield line
                                                                      ).toList 
                                                                   }

    def extraire_phrases(chemin_texte : String, chemin_dictionnaire : String) : List[Phrase] = {
      
        val texte = Source.fromFile(chemin_texte).getLines().filter(_!="").foldLeft(""){_+_}
        
        /*phrases du texte
          Remarque : ici, on utilise split_phrases, je ne modifie pas cette fonction car ce n'est pas demandé dans les consignes,
          mais son utilisation en splittant par :, =, (, ... peut être discutée, car le concept de phrase n'est pas forcément bon 
          et les poèmes alors générés peuvent être également revus...*/
        val phrases_txt = for {
                       phrase <- split_phrases(texte)
                    } yield phrase
        /* 
         * For comprehension : utilisation de structure de donnée (Set)...
         */
        val mots_set : Set[String] = (for {
                       mot <- split_mots(texte)
                    } yield mot).toSet 
                    
        /*
         * Liste de chaines de caractères représentant chaque ligne du dictionnaire
         * On n'extrait ici que les mots qui se trouvent dans mots_set pour ne pas
         * charger tout le dictionnaire en mémoire
         */
        val dico = lire_csv(chemin_dictionnaire, mots_set)
        
        /*
         * For comprehension : -utilisation de structure de données (Map et List)
         * -utilisation de méthode de haut niveau sur une liste (map sur dico)
         * -utilisation de fonction anonyme et fonction partielle
         * Table de hachage qui contient comme clés chacun des mots (String) du dictionnaire
         * Et comme valeur des Mot (voir classe Mot).
         */
        val mots_hachage : Map[String,Mot] = dico.map { line => ( (line.split(",")(1)), new Mot(line.split(",")(1), line.split(",")(6).toInt, line.split(",")(8)) )  }.toMap
        
             
        /*Liste des phrases du texte dont tous les mots font partie du dictionnaire*/
        val phrases = for {
            p<-phrases_txt  
            if (((split_mots(p) map (mots_hachage contains _)) forall (x=>x)) && p.trim!="")
        } yield Phrase(p.trim, mots_hachage)
        
        phrases.toList
        
    }
}

/*
 * Classe abstraite pour la création d'un poème
 */
abstract class Poeme(phrases:List[Phrase]) {
  
    /*Renvoie des phrases aléatoirement*/
    def choose():List[Phrase] = {
        for {i<-List.range(0,phrases.length)}
            yield phrases((new Random).nextInt.abs % phrases.length)
    }

    /* Renvoie au hasard des couples de phrases qui riment
     * difSyl : nombre de syllabes maximum de différence exigé pour choisir deux vers qui riment
     * (Paramétrisation afin de pouvoir décider en amont la précision sur le nombre de syllabes)
     */
    def choose_deux(difSyl : Int):List[(Phrase,Phrase)] = {
        Random.shuffle(for {
            p1<-phrases
            p2<-phrases if ((p1!=p2) && (p1.syllabes > 4 && p1.syllabes < 13 && p1.syllabes - difSyl <= p2.syllabes && p1.syllabes + difSyl >= p2.syllabes) && (p1 rime_avec p2))
        } yield (p1,p2))
    }

    /*Renvoie un poème*/
    def ecrire() : String
}

/*
 * Classe permettant de créer un poème de deux vers
 * For comprehension : héritage de la classe abstraite Poeme
 */
class DeuxVers(phrases:List[Phrase]) extends Poeme(phrases:List[Phrase]) {
   /*
    * Écrire un petit poème de deux vers seulement
    * Le nombre de syllabes maximum de différence entre deux vers qui riment est indiqué en paramètre de choose_deux 
    * (on aurait très bien pu encore paramétré pour choisir encore plus en amont...)
    * For comprehension : -utilisation de méthode de haut niveau sur liste (map)
    * -utilisation de structure de donnée (List)
    * -utilisation de fonction anonyme (pour découpler et afficher proprement les vers)
    */
   def ecrire() : String = choose_deux(1).map {case (x,y) => x + "\n" + y + "\n"}.head.toString
}
 
/*
 * Objet et méthode principaux permettant de lancer le programme de génération automatique de poèmes de 2 vers
 */
object Main {
    def main(args:Array[String]) {
        val chemin_corpus:String = "dixcontes.txt" //texte contenant des phrases utilisé pour sélectionner des vers qui riment
        val chemin_dictionnaire:String = "dicorimes.dmp" //dictionnaire contenant une liste de mots associés à leur nombre de syllabes et à leur phonétique.
        
        /*  Batterie de Test
        val mot = new Mot("vagabonder", 4, "vagab§de")
        val mot2 = new Mot("abriter", 3, "abRite")
        val salut = new Mot("salut", 2, "salu")
        println(mot.toString() + " et " + mot2.toString())
        println(mot.toPhone(mot.ph) + " et " + mot2.toPhone(mot2.ph))
        println("rime ? " + mot.rime_avec(mot2))
        
        val phr = new Phrase("salut vagabonder", Map(("salut", salut), ("vagabonder", mot)))
        val phr2 = new Phrase("salut abriter", Map(("salut", salut), ("abriter", mot2)))
        println(phr)
        println(phr.syllabes)
        println("rime ? " + phr.rime_avec(phr2))
        */
        
        //extraction des phrases du texte utilisée pour généré le poème
        val texte = Phrases.extraire_phrases(chemin_corpus, chemin_dictionnaire)
        
        //génération du poème de 2 vers aléatoire et affichage
        val poeme = new DeuxVers(texte)
        println(poeme.ecrire)
        
    } 
}

/*/*
 *Auteur: Keil Laurent
 *Date: 26 février 2015
 *Description: poèmes aléatoires exceptionnels
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
    def lire_csv (chemin:String, mots:Set[String]) : Try[List[String]] = Try({  
                                                                                (for {
                                                                                      line <- Source.fromFile(chemin).getLines()  
                                                                                      if mots contains line.split(",")(1)
                                                                                     } yield line
                                                                                ).toList 
                                                                            })
                                                                   
    def extraire_phrases(chemin_texte : String, chemin_dico : String) : Try[List[Phrase]] = {
      
        /*Liste des phrases du texte dont tous les mots font partie du dictionnaire*/
       for {
            texte <- Try(Source.fromFile(chemin_texte).getLines().filter(_!="").foldLeft(""){_+_})
            phrases_txt = split_phrases(texte)
            mots_set = split_mots(texte).toSet
            dico <- lire_csv(chemin_dico, mots_set)
            mots_hachage = dico.map { line => ( (line.split(",")(1)), new Mot(line.split(",")(1), line.split(",")(6).toInt, line.split(",")(8)) )  }.toMap
            phrases <- Try(phrases_txt filter (p => (((split_mots(p) map (mots_hachage contains _)) forall (x=>x)) && p.trim!="")) map (p => Phrase(p.trim, mots_hachage)))
       } yield phrases.toList
           
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
    
    /*//fonction qui génère des phrases qui riment avec p1 (avec un nombre de syllabes de différence max de 1)
    def list_phr_riment (p1 : Phrase) = for {
                            p2 <- phrases
                            if ((p1!=p2) &&  (p1 rime_avec p2) && (p1.syllabes - 1 <= p2.syllabes && p1.syllabes + 1 >= p2.syllabes))
                          } yield p2*/
    def existCoupleRime (p1 : Phrase) = (for {
                            p2 <- phrases
                            exist = ((p1!=p2) &&  (p1 rime_avec p2) && (p1.syllabes - 1 <= p2.syllabes && p1.syllabes + 1 >= p2.syllabes))
                          } yield exist) 
    
    //fonction pour savoir s'il existe au moins un couple de phrases qui riment avec quasi même nombre de syllabes
    def existCoupleRime = (for {
                            p1 <- phrases
                            p2 <- phrases
                            exist = ((p1!=p2) &&  (p1 rime_avec p2) && (p1.syllabes - 1 <= p2.syllabes && p1.syllabes + 1 >= p2.syllabes))
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
                    phrases flatMap (p1 => 
                                      phrases filter (p2 => ((p1!=p2) &&  (p1 rime_avec p2) && (p1.syllabes - 1 <= p2.syllabes && p1.syllabes + 1 >= p2.syllabes))) 
                                              map (p2 => (p1,p2))
                                  )
                )
             } 
          )
        }
    }

    /*Renvoie un poème*/
    def ecrire() : String
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
   def ecrire() : String = couple_riment match {
      case Some(x) => val coupleGen = x.generate.head ; coupleGen._1 + "\n" + coupleGen._2
      case None => "Il n'existe pas de couple de phrases qui riment et qui ont presque le même nombre de syllabes"
   }
}
 
/*
 * Objet et méthode principaux permettant de lancer le programme de génération automatique de poèmes de 2 vers
 */
object Main {
    def main(args:Array[String]) {
        val chemin_corpus:String = "download.txt" //texte contenant des phrases utilisé pour sélectionner des vers qui riment
        val chemin_dictionnaire:String = "dicorimes.dmp" //dictionnaire contenant une liste de mots associés à leur nombre de syllabes et à leur phonétique.
         
        val poeme = for {
            //extraction des phrases du texte utilisée pour généré le poème
            texte <- Phrases.extraire_phrases(chemin_corpus,chemin_dictionnaire)
        } yield new DeuxVers(texte)
        //génération du poème de 2 vers aléatoire et affichage du poeme si pas d'erreur, sinon affichage de l'erreur
        poeme match {
            case Success(lines) => println(lines.ecrire)
            case Failure(ex) => println("Erreur dans l'extraction de fichiers.")
        }
    } 
}*/

