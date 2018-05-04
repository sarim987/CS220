import scala.util.matching.Regex

object Regexes extends hw.regex.RegexLike {

  def notAlphanumeric: Regex = "[^a-zA-Z0-9]*".r
  def time: Regex = "(2[0-3]|[01]?[0-9]):([1-5]{1}[0-9])".r
  def phone: Regex = "[(][0-9]{3}[)] [0-9]{3}-[0-9]{4}".r
  def zip: Regex = "[0-9]{5}([-][0-9]{4})?".r
  def comment: Regex = """/\*.*\*/""".r
  def numberPhrase: Regex = "(twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)(-one|-two|-three|-four|-five|-six|-seven|-eight|-nine)?".r
  def roman: Regex = "X{0,3}(IX|IV|V?I{0,3})".r
  def date: Regex = "(([0-9]{2}(00|04|08|12|16|20|24|28|32|36|40|44|48|52|56|60|64|68|72|76|80|84|88|92|96))-02-(0[0-9]|[1-2][0-9]))|([0-9]{4}-(04|06|09|11)-(0[0-9]|[1-2][0-9]|30))|([0-9]{4}-(01|03|05|07|08|10|12)-(0[0-9]|[1-2][0-9]|30|31))|([0-9]{4}-02-(0[0-9]|1[0-9]|2[0-8]))".r
  def evenParity: Regex = """(([0|2|4|6|8]*[1|3|5|7|9]){2})*[0|2|4|6|8]*""".r // (b + ab*a)+
}