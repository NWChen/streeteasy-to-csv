abstract class BaseListing {
  def href: String
  def price: String
  def address: String
  def bedrooms: Double
  def bathrooms: Double
}

case class Listing(
  href: String,
  price: String,
  address: String,
  bedrooms: Double,
  bathrooms: Double,
  sqft: Double,
) extends BaseListing

// TODO unfuck
case class EnhancedListing(
  href: String,
  price: String,
  address: String,
  bedrooms: Double,
  bathrooms: Double,
  sqft: Double,
  dishwasherInUnit: Boolean,
  washerDryerInUnit: Boolean,
  walkToGoogle: String, // time in minutes
  transitToGoogle: String, // time in minutes
  walkToGS: String, // time in minutes
  transitToGS: String,
  walkToSIMON: String, // time in minutes
  transitToSIMON: String,
  nearestPark: String,
  nearestSupermarket: String,
  nearestLibrary: String,
  pictures: List[String]
)
extends BaseListing

// TODO make implicit
class CsvWrapper(val e: EnhancedListing) extends AnyVal {

  /*
  def toCsv(): String = List(e.href,
    e.price,
    e.address,
    e.bedrooms,
    e.bathrooms,
    e.sqft,
    e.pictures,
    e.dishwasherInUnit,
    e.washerDryerInUnit,
    e.walkToGoogle, // time in minutes
    e.transitToGoogle, // time in minutes
    e.walkToGS, // time in minutes
    e.transitToGS,
    e.walkToSIMON, // time in minutes
    e.transitToSIMON,
    e.nearestPark,
    e.nearestSupermarket,
    e.nearestLibrary).mkString(",")
   */

  // ???
  def toCsv(): String = e.productIterator.map {
    case Some(v: String) => v.replace(",", "") // to avoid separation on CSV
    case None => ""
    case rest => rest
  }.mkString(",").replaceAll("List\\(", "")
}