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
  "name": "Richmond Town Square",
  "city": "Richmond Heights",
  "state": "OH",
  "stars": 2,
  "review_count": 17,
  "attributes": {
    "RestaurantsPriceRange2": 2,
    "BusinessParking": {
      "garage": false,
      "street": false,
      "validated": false,
      "lot": true,
      "valet": false
    },
    "BikeParking": true,
    "WheelchairAccessible": true
  },
  "categories": [
    "Shopping",
    "Shopping Centers"
  ]
}
{
  "name": "South Florida Style Chicken & Ribs",
  "city": "Charlotte",
  "state": "NC",
  "stars": 4.5,
  "review_count": 4,
  "attributes": {
    "GoodForMeal": {
      "dessert": false,
      "latenight": false,
      "lunch": false,
      "dinner": false,
      "breakfast": false,
      "brunch": false
    },
    "HasTV": false,
    "RestaurantsGoodForGroups": true,
    "NoiseLevel": "average",
    "RestaurantsAttire": "casual",
    "RestaurantsReservations": false,
    "OutdoorSeating": false,
    "BusinessAcceptsCreditCards": false,
    "RestaurantsPriceRange2": 2,
    "RestaurantsDelivery": true,
    "Ambience": {
      "romantic": false,
      "intimate": false,
      "classy": false,
      "hipster": false,
      "divey": false,
      "touristy": false,
      "trendy": false,
      "upscale": false,
      "casual": false
    },
    "RestaurantsTakeOut": true,
    "GoodForKids": true
  },
  "categories": [
    "Food",
    "Soul Food",
    "Convenience Stores",
    "Restaurants"
  ]
}
    """)
    val res2 = JsonHelper.parse(""" 
  {
    "categories": ["Food", "Fun"]
  }
""")
    assert(category(List(res), "Food") == List(res2))

  }
}
