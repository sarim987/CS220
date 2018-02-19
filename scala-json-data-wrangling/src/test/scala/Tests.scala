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
    assert(Some(JsonString("CA")) == key(res, "state"))
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
    assert(arrToString(List(JsonString("Bowling"), JsonString("Fun")), JsonString("Taco Bell")) 
    == List(("Bowling", JsonString("Taco Bell")), ("Fun", JsonString("Taco Bell"))))

  }
/**
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

    test("groupbycat2") {
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



    assert(groupByCategory(List(res)) == 
      Map("Food" -> List(("Taco Bell")), 
        "Fun" -> List(("Taco Bell")) ))
  }
**/
    test("best test 1") {
      val myJson: Json = JsonHelper.parse(""" { "stars": 3, "review_count": 23, "name": "ResturantMA" }""")
      val moreJson2: Json = JsonHelper.parse(""" { "stars": 1, "review_count": 35, "name": "ResturantCT" }""")
      val moreJson3: Json = JsonHelper.parse(""" { "stars": 4.9, "review_count": 53, "name": "ResturantCA" }""")
      val moreJson4: Json = JsonHelper.parse(""" { "stars": 2, "review_count": 45, "name": "ResturantMA" }""")
      val moreJson5: Json = JsonHelper.parse(""" { "stars": 4.9, "review_count": 331, "name": "ResturantFL" }""")
      val jsonList: List[Json] = List(myJson, moreJson2, moreJson3, moreJson4, moreJson5)

      //assert(isFromState(myJson, "MA"))
      assert(bestPlace(jsonList) == Some(JsonDict(Map(JsonString("stars") -> JsonNumber(4.9), 
      JsonString("review_count") -> JsonNumber(331.0), JsonString("name") -> JsonString("ResturantFL")))))
  }





    test("group by state test 1") {
      val myJson: Json = JsonHelper.parse(""" { "state": "MA", "name": "ResturantMA" }""")
      val moreJson2: Json = JsonHelper.parse(""" { "state": "CT", "name": "ResturantCT" }""")
      val moreJson3: Json = JsonHelper.parse(""" { "state": "CA", "name": "ResturantCA" }""")
      val moreJson4: Json = JsonHelper.parse(""" { "state": "MA", "name": "ResturantMA" }""")
      val moreJson5: Json = JsonHelper.parse(""" { "state": "FL", "name": "ResturantFL" }""")
      val jsonList: List[Json] = List(myJson, moreJson2, moreJson3, moreJson4, moreJson5)

      //assert(isFromState(myJson, "MA"))
      assert(groupByState(jsonList) == 
        Map("MA" -> List(myJson, moreJson4), 
            "CT" -> List(moreJson2),
            "CA" -> List(moreJson3),
            "FL" -> List(moreJson5)
            ))
  }






   test("group by state test 2") {
      val myJson: Json = JsonHelper.parse("""{"attributes":{ "Ambience": { "Fun": true, "Romantic": false }, "name": "ResturantMA" } }""")
      //val myJson2: Json = JsonHelper.parse("""{"attributes":{ "Ambience": { "Fun": true, "Romantic": false }, "name": "ResturantTWOO" } }""")
      //val myJson3: Json = JsonHelper.parse("""{"attributes":{  "name": "THREEResturant" } }""")
      val jsonList: List[Json] = List(myJson)

      //assert(isFromState(myJson, "MA"))
      assert(hasAmbience(jsonList, "Fun") == List(JsonDict(Map(JsonString("attributes") -> JsonDict(Map(JsonString("Ambience") -> JsonDict(Map(JsonString("Fun") -> JsonBool(true), JsonString("Romantic") -> JsonBool(false))), JsonString("name") -> JsonString("ResturantMA")))))))
  }
  
}
