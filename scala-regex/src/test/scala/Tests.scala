import Regexes._

class TrivialTestSuite extends org.scalatest.FunSuite {

  test("The Regexes object must be defined") {
    val regexes: hw.regex.RegexLike = Regexes
  }
  test("notAlpha") {
    assert(notAlphanumeric.pattern.matcher("$^%&&*(").matches())
    assert(notAlphanumeric.pattern.matcher("dcscc00877").matches() == false)
    assert(notAlphanumeric.pattern.matcher("sfsa789^*&").matches() == false)
  }
  test("time") {
    assert(time.pattern.matcher("21:29").matches())
    assert(time.pattern.matcher("11:29").matches())
    assert(time.pattern.matcher("01:60").matches() == false)
  }
  test("phone") {
    assert(phone.pattern.matcher("(413) 213-2131").matches())
    assert(phone.pattern.matcher("(413)213-2131").matches() == false)
    assert(phone.pattern.matcher("232-334-1122").matches() == false)
  }  
  test("zip") {
    assert(zip.pattern.matcher("01002-1234").matches())
    assert(zip.pattern.matcher("01002-3445").matches())
    assert(zip.pattern.matcher("31124-3223").matches())
    assert(zip.pattern.matcher("443244 1234").matches() == false)
  }
  test("comment") {
    assert(comment.pattern.matcher("/*Hello this is a comment.*/").matches())
    assert(comment.pattern.matcher("/*this comment is not finished").matches() == false)
    assert(comment.pattern.matcher("this comment is not finished*/").matches() == false)
  }  
  test("numberPhrase") {
    assert(numberPhrase.pattern.matcher("twenty").matches())
    assert(numberPhrase.pattern.matcher("twenty-zero").matches() == false)
  }  
  test("roman") {
    assert(roman.pattern.matcher("XXXIX").matches())
    assert(roman.pattern.matcher("XIIIIIII").matches() == false)
  }  
  test("date") {
    assert(date.pattern.matcher("2016-02-29").matches())
    assert(date.pattern.matcher("2017-02-29").matches() == false)
    assert(date.pattern.matcher("2016-04-31").matches() == false)
  }  
  test("evenParity") {
    assert(evenParity.pattern.matcher("224").matches())
    assert(evenParity.pattern.matcher("999").matches() == false)
    assert(evenParity.pattern.matcher("214").matches() == false)
  }  
}
