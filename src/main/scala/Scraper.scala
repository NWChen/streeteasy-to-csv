import cats.Traverse
import io.circe.Decoder.Result
import io.circe.generic.decoding.DerivedDecoder.deriveDecoder
import io.circe.{ACursor, Decoder, HCursor, Json}
import io.circe.jawn.{decode, parse}
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.elementList
import net.ruippeixotog.scalascraper.util.ProxyUtils
import scalaj.http.Http
import scala.language.postfixOps

class Scraper {
  //val USER_AGENT = "Mozilla"
  //val USER_AGENT = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.95 Safari/537.11"
  val USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.114 Safari/537.36"
  val browser: Browser = new JsoupBrowser(userAgent=USER_AGENT)
  val root: String = "https://streeteasy.com"
  //val target: String = root + "/3-bedroom-apartments-for-rent/manhattan/price:-6000"
  val target: String = root + "/3-bedroom-apartments-for-rent/nyc/status:open%7Cprice:-6000%7Carea:102,119,144,135%7Cbaths%3E=1%7Camenities:dishwasher,washer_dryer"

  def getListings(page: Int = 1) = {
    println(s"Getting listings for target: $target")
    val suffix = if (page > 1) s"?page=$page" else ""
    val doc = browser.get(target + suffix)
    val listings = doc >?> elementList("ul article") getOrElse List()

    for {
      li <- listings
      details = (li >> element(".details_info") >> texts("li")).toList

      href = (li >> element(".details-titleLink")).attr("href")
      price = li >> text(".price-info .price")
      address = li >> text(".details-title a")

      bedroomsText = (details filter (_.contains("bed"))).head
      bedrooms = bedroomsText.replaceAll("\\s+bed(s*)", "").toDouble

      bathroomsText = (details filter (_.contains("bath"))).head
      bathrooms = bathroomsText.replaceAll("\\s+bath(s*)", "").toDouble

      sqftText = (details filter(_.contains("ft"))).headOption
      sqft = sqftText match {
        case Some(s) => s.replaceAll("\\s+ft²", "")
          .replaceAll(",", "").toDouble
        case None => -1
      }

    } yield new Listing(
      href = root + href,
      address = address,
      price = price,
      bedrooms = bedrooms,
      bathrooms = bathrooms,
      sqft = sqft,
    )
  }

  def getEnhancedListing(listing: Listing)(implicit apiKey: String): EnhancedListing = {
    println(s"Enhancing listing for address: ${listing.address}")

    val doc = browser.get(listing.href)
    val pictures = for {
      imgLi <- doc >?> elementList(".Flickity-list li") getOrElse List()
      imgHref = imgLi.attr("data-src")
    } yield imgHref

    // TODO messy
    // bring google maps to head, if possible
    val mapPicture: Option[String] = pictures filter (_.contains("maps.google")) headOption // or just `find`
    val orderedPictures = mapPicture match {
      case Some(pic) => pic :: pictures.tail
      case None => pictures
    }

    val amenities = doc >?> elementList(".AmenitiesBlock-list .AmenitiesBlock-item") getOrElse List() map (_.innerHtml)
    val washerDryerInUnit = amenities filter (a => a.contains("ryer") || a.contains("aundry")) nonEmpty
    val dishwasherInUnit = amenities filter (_.contains("ishwasher")) nonEmpty

    // TODO clean this up
    val GOOGLE = "111 8th Ave, New York, NY 10011"
    val GS = "200 West Street, New York, NY 10282"
    val SIMON = "125 W 25th Street, New York, NY 10001"
    val addr: String = listing.address

    new EnhancedListing(
      href = listing.href,
      address = listing.address,
      price = listing.price,
      bedrooms = listing.bedrooms,
      bathrooms = listing.bathrooms,
      sqft = listing.sqft,
      pictures = orderedPictures,
      transitToGoogle = this.getCommute(addr, GOOGLE),
      transitToGS = this.getCommute(addr, GS),
      transitToSIMON = this.getCommute(addr, SIMON),
      walkToGoogle = this.getCommute(addr, GOOGLE, TransitMode.Walking),
      walkToGS = this.getCommute(addr, GS, TransitMode.Walking),
      walkToSIMON = this.getCommute(addr, SIMON, TransitMode.Walking),
      nearestPark = this.getNearest(addr, PlaceType.Park),
      nearestSupermarket = this.getNearest(addr, PlaceType.Supermarket),
      nearestLibrary = this.getNearest(addr, PlaceType.Library),
      washerDryerInUnit = washerDryerInUnit,
      dishwasherInUnit = dishwasherInUnit,
    )
  }

  // Converts an address into a (lat, lng) position
  private def geocode(address: String)(implicit apiKey: String): Location = {
    val url = "https://maps.googleapis.com/maps/api/geocode/json"
    val resp = Http(url)
      .param("address", address)
      .param("key", apiKey)
      .asString
      .body.stripMargin
    val cursor: HCursor = parse(resp).getOrElse(Json.Null).hcursor
    val ac: ACursor = cursor.downField("results").downArray.downField("geometry").downField("location")

    new Location(
      lat = ac.get[Double]("lat").getOrElse(0),
      lng = ac.get[Double]("lng").getOrElse(0)
    )
  }

  private def round(n: Double, places: Double = 5): Double = {
    val factor = math.pow(10, places)
    (n * factor).floor / factor
  }

  // Get nearest (to origin address) prominent place of type $placeType
  def getNearest(originStr: String, placeType: String)(implicit apiKey: String): String = { //(origin: String, destination: String) = {
    val origin: Location = geocode(originStr)(apiKey)
    val (lat, lng) = (round(origin.lat), round(origin.lng))
    val url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
    val resp = Http(url)
      .param("location", s"$lat,$lng")
      //.param("radius", radius)
      .param("rankby", "distance") // TODO prominence instead
      .param("type", placeType)
      .param("key", apiKey)
      .asString
      .body.stripMargin
    val cursor: HCursor = parse(resp).getOrElse(Json.Null).hcursor
    val ac: ACursor = cursor.downField("results").downArray

    ac.get[String]("name").getOrElse("")
  }

  def getCommute(originStr: String, destinationStr: String,
                 transitMode: String = TransitMode.Train)(implicit apiKey: String) = {
    // TODO figure out how to use implicits correctly
    val url = "https://maps.googleapis.com/maps/api/distancematrix/json"
    val resp = Http(url)
      .param("origins", originStr)
      .param("destinations", destinationStr)
      .param("mode", transitMode)
      .param("key", apiKey)
      .asString
      .body.stripMargin
    val cursor: HCursor = parse(resp).getOrElse(Json.Null).hcursor
    val ac: ACursor = cursor.downField("rows")
      .downArray.downField("elements")
      .downArray.downField("duration")

    ac.get[String]("text").getOrElse("")
  }
}
