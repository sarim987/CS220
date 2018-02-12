import hw.json._
import hw.wrangling.WranglingLike

object Wrangling extends WranglingLike {

  val data: List[Json] = JsonHelper.fromFile("yelp.json")

  def key(json: Json, key: String): Option[Json] = json match {
    case JsonDict(aMap) => aMap.get(JsonString(key)) match {
      case Some(stateName) => stateName match {
      case JsonString(x) => Some(stateName)
      case _ => None
      }
      case _ => None
    }
    case _ => None

  }

  def isFromState(datum: Json, state: String): Boolean = datum match {
    case JsonDict(aMap) => aMap.get(JsonString("state")) match {
      case None => false
      case Some(stateName) => stateName match {
        case JsonString(x) => x == state
        case _ => false
      }
    }
    case _ => false
  }

  def fromState(data: List[Json], state: String): List[Json] = {
    data.filter(datum => isFromState(datum, state))
  }

  def ratingHelper(datum: Json, ratingScore: Double, func: (Double, Double) => Boolean): Boolean = datum match {
  case JsonDict(aMap) => aMap.get(JsonString("stars")) match {
    case None => false
    case Some(rating) => rating match {
      case JsonString(x) => func(x.toDouble, ratingScore)
      case _ => false
    }
  }
  case _ => false
}
  def ratingLT(data: List[Json], rating: Double): List[Json] = {
    def lt(x: Double, y: Double): Boolean = x <= y
    data.filter(data => ratingHelper(data, rating, lt))
  }

  def ratingGT(data: List[Json], rating: Double): List[Json] = {
    def gt(x: Double, y: Double): Boolean = x >= y
    data.filter(data => ratingHelper(data, rating, gt))
  }

  def catHelper(datum: Json, key: String, func: (String, String) => Boolean): Boolean = datum match {
  case JsonDict(aMap) => aMap.get(JsonString("categories")) match {
    case None => false
    case Some(categ) => categ match {
      case JsonArray(x) => x.contains(JsonString(key))
      case _ => false
    }
  }
  case _ => false
}

  def category(data: List[Json], categor: String): List[Json] = {
    def eq(x: String, y: String): Boolean = x == y
    data.filter(data => catHelper(data, categor, eq))
  }

  def groupByState(data: List[Json]): Map[String, List[Json]] =
    data.groupBy(datum => datum match {
      case JsonDict(aMap) => aMap.get(JsonString("state")) match {
        case Some(JsonString(stateCode)) => stateCode
        case _ => "no state"
      }
      case _ => "no state"
  })

  def groupByCategory(data: List[Json]): Map[String, List[Json]] =
    data.groupBy(datum => datum match {
      case JsonDict(aMap) => aMap.get(JsonString("categories")) match {
        case Some(JsonArray(x)) => x match {
          case JsonString(item) :: Nil => item
          case JsonString(item) :: tail => item
          case _ => "no state"
        }

        case _ => "no state"
      }
      case _ => "no state"
  })

  def bestPlace(data: List[Json]): Option[Json] = ???

  def hasAmbience(data: List[Json], ambience: String): List[Json] = ???

}
