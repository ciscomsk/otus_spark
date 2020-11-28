package Lesson_1

object Less_1_check extends App {

  import scala.io.{BufferedSource, Source}

  val source: BufferedSource =
    Source.fromURL("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2020.csv")

  def headers(head: String): Map[String, Int] =
    head.split(",").zipWithIndex.toMap

  val head :: lines = source.getLines.toList
  val headerMap: Map[String, Int] = headers(head)

  val t_id: Int = headerMap("tourney_id")

  /**
   * Количество побед спортсмена в разрезе по турнирам
   */
  def winsInTourneysByName(winnerName: String): Map[String, Int] = {
    val winner: Int = headerMap("winner_name")

    val winsInMatches: List[String] = lines.filter(x => x.split(",")(winner).contains(winnerName))

    val winsByTourneys: Map[String, Int] = winsInMatches
      .map(x => x.split(","))
      .map(x => (x(t_id), 1))
      .groupBy(x => x._1)
      .map(x => (x._1, x._2.size))

    winsByTourneys
  }

  println(winsInTourneysByName("Daniil Medvedev"))

  /**
   * Количество различных турниров в разрезе по месяцам
   */
  def distinctTourneysByMonth: Map[String, Int] = {
    val t_date: Int = headerMap("tourney_date")

    val distinctTourneysById: List[String] = lines.distinctBy(x => x.split(",")(0))

    val tourneysByMonth: Map[String, Int] = distinctTourneysById
      .map(x => x.split(","))
      .map(x => (x(t_date).slice(4, 6), x(t_id)))
      .groupBy(x => x._1)
      .map(x => (x._1, x._2.size))

    tourneysByMonth
  }

  println(distinctTourneysByMonth)

}
