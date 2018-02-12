import Wrangling._
import hw.json._

// You will need to write more tests.
class TestSuite extends org.scalatest.FunSuite {

  test("Empty test works") {
    assert(true)
  }

  test("key Test 1") {
    val res = JsonHelper.parse(""" 
  {
    "state": "CA" 
  }
""")
    assert(key(res, "state") == Some(JsonString("CA")))

  }

  test("fromState") {
    val myJson: Json = JsonDict(Map(JsonString("state") -> JsonString("MA")))
    val moreJson: Json = JsonDict(Map(JsonString("state") -> JsonString("NJ")))
    val jsonList: List[Json] = List(myJson, moreJson)

    assert(isFromState(myJson, "MA"))
    assert(fromState(jsonList, "NJ") == List(moreJson))
  }
  test("ratingLT test 1") {
    val myJson: Json = JsonDict(Map(JsonString("stars") -> JsonString("3")))
    val moreJson: Json = JsonDict(Map(JsonString("stars") -> JsonString("2")))
    val jsonList: List[Json] = List(myJson, moreJson)

    //assert(isFromState(myJson, "MA"))
    assert(ratingLT(jsonList, 3) == List(myJson, moreJson))
  }
    test("ratingGT test 1") {
    val myJson: Json = JsonDict(Map(JsonString("stars") -> JsonString("3")))
    val moreJson: Json = JsonDict(Map(JsonString("stars") -> JsonString("4.5")))
    val jsonList: List[Json] = List(myJson, moreJson)

    //assert(isFromState(myJson, "MA"))
    assert(ratingGT(jsonList, 4.1) == List(moreJson))
  }

  test("categ test 1") {
    val res = JsonHelper.parse(""" 
  {
    "categories": ["Food", "Fun"]
  }
""")
    //val moreJson: Json = JsonDict(Map(JsonString("categories") -> Map(JsonString("Food"), JsonString("Fun"), JsonString("Games") ) ))
    //val jsonList: List[Json] = List(moreJson)
    assert(category(List(res), "Food") == List(res))

  }
  test("toStringfromJsonString"){
    assert(arrToString(List(JsonString("Bowling"), JsonString("Fun"))) == List("Bowling", "Fun"))

  }

  test("groupbycat") {
      val res = JsonHelper.parse(""" 
  {
    "name": "Taco Bell",
    "categories": ["Food", "Fun"]
  }
""")
      val res2 = JsonHelper.parse(""" 
  {
    "name": "Taco Bell",
    "categories": ["Food", "Fun"]
  }
""")



    assert(categorize(res) == 
      Map("Food" -> List(("Taco Bell")), 
        "Fun" -> List(("Taco Bell")) ))

  }
  
}
