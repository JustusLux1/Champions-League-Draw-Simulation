import scala.collection.mutable
import scala.util.Random

object ChampionsLeagueDraw extends App {
  /**
   *
   * @param name          Name des Klubs
   * @param country       Abkürzung des Heimatlandes des Klubs
   * @param group         Buchstabe der Gruppe
   * @param finishedFirst True, wenn der Klub erster wurde, sonst false
   */
  final case class Klub(
                         name: String,
                         country: String,
                         group: Char,
                         finishedFirst: Boolean
                       )

  // Ergebnisse nach der Gruppenphase 22/23
  val klubs = List(
    Klub("SSC Napoli",        "IT", 'A', true),
    Klub("Liverpool FC",      "EN", 'A', false),
    Klub("FC Porto",          "PT", 'B', true),
    Klub("Club Brugge",       "BE", 'B', false),
    Klub("Bayern",            "DE", 'C', true),
    Klub("FC Internazionale", "IT", 'C', false),
    Klub("Tottenham Hotspurs","EN", 'D', true),
    Klub("Eintr. Frankfurt",  "DE", 'D', false),
    Klub("Chelsea FC",        "EN", 'E', true),
    Klub("AC Milan",          "IT", 'E', false),
    Klub("Real Madrid",       "ES", 'F', true),
    Klub("RB Leipzig",        "DE", 'F', false),
    Klub("Manchester City",   "EN", 'G', true),
    Klub("Bor. Dortmund",     "DE", 'G', false),
    Klub("Benfica",           "PT", 'H', true),
    Klub("PSG",               "FR", 'H', false)
  )

  var firstPlaces  = List[Klub]()
  var secondPlaces = List[Klub]()

  for (team <- klubs) {
    if (team.finishedFirst) {
      firstPlaces = firstPlaces :+ team
    } else {
      secondPlaces = secondPlaces :+ team
    }
  }
  // Mapping von jedem Gruppensieger auf ein Mapping was für jeden Gruppenzweiten
  // speichert wie häufig er in der Simulation auf den jeweiligen Gruppenersten getroffen ist
  val countMatchesDraw           = mutable.Map[String, mutable.Map[String, Int]]()
  val countMatchesBacktrack      = mutable.Map[String, mutable.Map[String, Int]]()
  val countMatchesRealSimulation = mutable.Map[String, mutable.Map[String, Int]]()

  // Initialisierung der äußeren Map, jede Map kriegt als keys die Gruppenersten.
  // Die jeweiligen Values sind mit Standardwert 0 belegt.
  for (winner <- firstPlaces) {
    countMatchesDraw.put(winner.name,           mutable.Map[String, Int]().withDefaultValue(0))
    countMatchesBacktrack.put(winner.name,      mutable.Map[String, Int]().withDefaultValue(0))
    countMatchesRealSimulation.put(winner.name, mutable.Map[String, Int]().withDefaultValue(0))
  }

  val random = new Random

  /**
   * Simuliert die Auslosung des Champions League AF mit Hilfe von backtracking
   *
   * @param winners  Liste der noch nicht gezogenen Gruppensieger
   * @param seconds  Liste der noch nicht gezogenen Gruppenzweiten
   * @param pairings Bisherige gültige Paarungen
   * @return         Entweder None wenn keine Paarungen aufgrundlage der Einschränkungen gefunden worden sind, oder die Liste der Paarungnen für das ausgeloste AF
   */
  def backtrack(
                 winners: List[Klub],
                 seconds: List[Klub],
                 pairings: List[(Klub, Klub)]
               ): Option[List[(Klub, Klub)]] = {
    if (winners.isEmpty) {
      Some(pairings)
    } else {
      val winner = winners.head
      val drawableSeconds = random.shuffle(
        seconds.filter(s => s.group != winner.group && s.country != winner.country)
      )

      drawableSeconds match {
        case Nil => None
        case _ =>
          drawableSeconds.foldLeft(Option.empty[List[(Klub, Klub)]]) { (acc, secondPlace) =>
            acc match {
              case Some(value) => Some(value)
              case None        =>
                backtrack(
                  winners.tail,
                  seconds.filter(_ != secondPlace),
                  pairings :+ (winner, secondPlace)
                )
            }
          }
      }
    }
  }

  var countAllPossibleMatchups = mutable.Buffer[List[(String, String)]]()

  /**
   * Die Funktion war ein Versuch alle möglichen Paarungen aufzuzählen und dadurch die Wahrscheinlichkeiten zu berechnen.
   * Besonderer Fokus liegt bei diesen Auslosungen auch darauf, dass die Wahrscheinlichkeiten sich durch die Reihenfolge verändern
   * könnten.
   *
   * @param winners  Liste der noch nicht gezogenen Gruppensieger
   * @param seconds  Liste der noch nicht gezogenen Gruppenzweiten
   * @param pairings Bisherige gültige Paarungen
   */
  def enumerateAllPossiblePaths(
                                 winners: List[Klub],
                                 seconds: List[Klub],
                                 pairings: List[(String, String)]
                               ): Unit = {
    if (winners.isEmpty) {
      // die Variable ist aufgrund des Backtrackings aus Komplexitätsgründen außerhalb der Funktion deklariert.
      countAllPossibleMatchups = countAllPossibleMatchups :+ pairings
    } else {
      val winner = winners.head
      val drawableSeconds = random.shuffle(
        seconds.filter(s => s.group != winner.group && s.country != winner.country)
      )

      for (second <- drawableSeconds) {
        enumerateAllPossiblePaths(
          winners.tail,
          seconds.filter(_ != second),
          pairings :+ (winner.name, second.name)
        )
      }
    }
  }

  /**
   * Hilfsfunktion zur Berechnung ob eine mögliche Konstellation an winners und seconds noch zu einer erfolgreichen Auslosung führen können
   *
   * @param winners Die verbleibenden Gruppenersten
   * @param seconds Die verbleibenden Gruppenzweiten
   * @return        True wenn die Auslosung zu einer möglichen Paarungen führen kann, sonst false
   */
  def isDrawStillPossible(winners: List[Klub], seconds: List[Klub]): Boolean = {
    if (winners.isEmpty) return true

    winners.exists { winner =>
      val remainingWinners = winners.filterNot(_ == winner)
      val drawableSeconds  = seconds.filter(s => s.group != winner.group && s.country != winner.country)

      drawableSeconds.exists { second =>
        val remainingSeconds = seconds.filterNot(_ == second)
        isDrawStillPossible(remainingWinners, remainingSeconds)
      }
    }
  }

  /**
   * Mit dieser Methode wird eine reale Auslosung simuliert und daher sollte diese Funktion die besten Auskünfte über die echten Wahrscheinlichkeiten geben
   *
   * @return Die Paarungen der Ziehung als Liste
   */
  def realSimulation(): List[(String, String)] = {
    val shuffled = random.shuffle(firstPlaces)
    var remainingWinners = shuffled
    var availableSeconds = secondPlaces
    var pairings: List[(String, String)] = Nil

    for (winner <- shuffled) {
      remainingWinners = remainingWinners.tail

      val possibleSeconds = availableSeconds.filter(s =>
        s.group != winner.group && s.country != winner.country
      )

      val drawable = possibleSeconds.filter { sec =>
        isDrawStillPossible(remainingWinners, availableSeconds.filterNot(_ == sec))
      }

      if (drawable.isEmpty) return Nil

      val drawn = drawable(random.nextInt(drawable.length))
      pairings ::= (winner.name -> drawn.name)
      availableSeconds = availableSeconds.filterNot(_ == drawn)
    }

    pairings.reverse
  }

  /**
   * Einfache Simulation, sollte man in einer Ziehung zu einem Deadlock kommen stoppt die Simulation ohne Endergebnis
   *
   * @return None, wenn es zu einem Deadlock gekommen ist. Sonst die gültigen Paarungen als Liste
   */
  def draw(): Option[List[(String, String)]] = {
    var pairings = List[(String, String)]()
    val shuffledFirstPlaces     = random.shuffle(firstPlaces)
    var availableSecondPlaces   = secondPlaces

    for (winner <- shuffledFirstPlaces) {
      val drawableSecondPlaces = availableSecondPlaces.filter(s =>
        s.group != winner.group && s.country != winner.country
      )

      if (drawableSecondPlaces.isEmpty) {
        return None
      } else {
        val drawnSecondPlace = drawableSecondPlaces(random.nextInt(drawableSecondPlaces.length))
        pairings = pairings :+ (winner.name, drawnSecondPlace.name)
        availableSecondPlaces = availableSecondPlaces.filter(_ != drawnSecondPlace)
      }
    }

    Some(pairings)
  }

  // Ab hier startet der Test der einzelnen Methoden

  var failedDraws     = 0
  val numberOfDraws   = 1000000

  // Es wird die backtrack, die realSimulation und die draw Funktion jeweils einmal im Schleifendurchgang durchgeführt.
  // Die erfolgreichen Ziehungen werden dann in den jeweiligen Zählstrukturen eingetragen.
  for (_ <- 0 to numberOfDraws) {

    draw() match {
      case Some(pairings) =>
        for ((first, second) <- pairings) {
          countMatchesDraw(first)(second) += 1
        }
      case None =>
        failedDraws += 1
    }

    backtrack(random.shuffle(firstPlaces), secondPlaces, Nil) match {
      case Some(pairings) =>
        for ((first, second) <- pairings) {
          countMatchesBacktrack(first.name)(second.name) += 1
        }
      case None => ()
    }

    realSimulation() match {
      case pairings =>
        for ((first, second) <- pairings) {
          countMatchesRealSimulation(first)(second) += 1
        }
    }
  }

  // Durch den Abbruch der draw Methode bei Deadlocks muss die erfolgreiche Anzahl an Ziehungen ermittelt werden
  val successfullDraws = numberOfDraws - failedDraws

  println("\n\nErgebnisse der Draw Funktion, Deadlocks werden nicht beachtet\n")
  for (first <- countMatchesDraw) {
    println(s"${first._1} ->")
    val sorted = first._2.toList.sortBy(_._2 * -1)
    for ((second, count) <- sorted) {
      val percentage = count.toDouble / successfullDraws * 100
      println(f"$second: $percentage%6.2f%%")
    }
    println()
  }

  println("\n\nErgebnisse der Backtrack Methode\n")
  for (first <- countMatchesBacktrack) {
    println(s"${first._1} ->")
    val sorted = first._2.toList.sortBy(_._2 * -1)
    for ((second, count) <- sorted) {
      val percentage = count.toDouble / numberOfDraws * 100
      println(f"$second: $percentage%6.2f%%")
    }
    println()
  }

  println(failedDraws)


  println("\n\nErgebnisse der RealSimulation Methode, Deadlocks werden während der Simulation verhindert\n")
  for (first <- countMatchesRealSimulation) {
    println(s"${first._1} ->")
    val sorted = first._2.toList.sortBy(_._2 * -1)
    for ((second, count) <- sorted) {
      val percentage = count.toDouble / numberOfDraws * 100
      println(f"$second: $percentage%6.2f%%")
    }

  }
}
