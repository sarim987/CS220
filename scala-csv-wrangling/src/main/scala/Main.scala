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

  def count(rows: List[SSARow]): Int = {
    rows.map(_.count).sum
  }

  def countGirlsAndBoys(rows: List[SSARow]): (Int, Int) = {
    (rows.filter(item => item.gender == Female()).map(_.count).sum, rows.filter(item => item.gender == Male()).map(_.count).sum)
  }

  def genderNeutralNames(rows: List[SSARow]): Set[String] = {
    rows.groupBy(item => item.name).mapValues(vals => vals.map(_.gender)).filter({case (a, b) => b == List(Male(), Female()) || b == List(Female(), Male())}).toList.map(_._1).toSet
  }

  def expectedAlive(gender: Gender, birthYear: Int, currentYear: Int,
    lifeExpectancies: List[CDCRow]): Boolean = {
    val a = lifeExpectancies.filter(item => item.birthYear == birthYear)
    if(a.length > 0){ 
      val b = a.head
      if(gender == Male()) (currentYear - birthYear) <= b.maleLifeExpectancy else (currentYear - birthYear) <= b.femaleLifeExpectancy
    } else false
  }

  def estimatePopulation(rows: List[SSARow], year: Int,
    lifeExpectancies: List[CDCRow]): Int = {
      count(rows.filter(item => expectedAlive(item.gender, item.birthYear, year, lifeExpectancies)))
    }

}