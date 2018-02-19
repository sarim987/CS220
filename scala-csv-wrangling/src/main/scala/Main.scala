import  hw.csv._

sealed trait Gender
case class Male() extends Gender
case class Female() extends Gender

case class SSARow(birthYear: Int, name: String, gender: Gender, count: Int)

case class CDCRow(birthYear: Int, maleLifeExpectancy: Int,
  femaleLifeExpectancy: Int)

object Main {
  def stringToGender(s : String) : Gender = s match { case "M" => Male() case "F" => Female() }

  def readSSARow(row: List[String]): SSARow = row match{
    case List(w, x, y, z) => SSARow(w.toInt, x, stringToGender(y), z.toInt)
    case _ => ???
  }

  def readCDCRow(row: List[String]): CDCRow = row match{
    case List(w, x, y) => CDCRow(w.toInt, x.toInt, y.toInt)
    case _ => ???
  }

  def yearIs(rows: List[SSARow], bound: Int): List[SSARow] = {
    def func(s : SSARow): Boolean = (s.birthYear == bound)
    rows.filter(func)
  }

  def yearGT(rows: List[SSARow], bound: Int): List[SSARow] = {
    def func(s : SSARow): Boolean = (s.birthYear > bound)
    rows.filter(func)
  }

  def yearLT(rows: List[SSARow], bound: Int): List[SSARow] = {
    def func(s : SSARow): Boolean = (s.birthYear < bound)
    rows.filter(func)
  }

  def onlyName(rows: List[SSARow], name: String): List[SSARow] = {
    def func(s : SSARow): Boolean = (s.name == name)
    rows.filter(func)
  }

  def mostPopular(rows: List[SSARow]): (String, Int) = {
    rows.groupBy(item => item.name).mapValues(vals => vals.map(_.count).sum).maxBy(_._2)
  }
  def findMax(rows: List[SSARow]): (String, Int) = {
    def findMaxHelper(max: (String, Int), lis: List[SSARow]): (String, Int) = lis match {
      case Nil => max
      case item :: tail if item.name == max._1 => findMaxHelper((max._1, max._2 + item.count), tail)
      case item :: tail if item.count > max._2 => findMaxHelper((item.name, item.count), tail)
      case item :: tail => findMaxHelper((max._1, max._2), tail)
    }
    findMaxHelper(("", 0), rows)
  }

  def count(rows: List[SSARow]): Int = ???

  def countGirlsAndBoys(rows: List[SSARow]): (Int, Int) = ???

  def genderNeutralNames(rows: List[SSARow]): Set[String] = ???

  def expectedAlive(gender: Gender, birthYear: Int, currentYear: Int,
    lifeExpectancies: List[CDCRow]): Boolean = ???

  def estimatePopulation(rows: List[SSARow], year: Int,
    lifeExpectancies: List[CDCRow]): Int = ???

}