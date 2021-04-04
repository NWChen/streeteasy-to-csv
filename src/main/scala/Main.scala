import java.io.{File, PrintWriter}

import net.ruippeixotog.scalascraper.util.ProxyUtils

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {

    // just load in a config next time
    val key = Source.fromFile("/home/neil/Desktop/automation/scala/apt/src/main/scala/apikey").mkString.stripSuffix("\n")

    val s = new Scraper
    val pw = new PrintWriter(new File("out.csv"))
    val TOP = 5 //19

    for {
      page <- (2 to TOP) //  TODO automate this
      listings = s.getListings(page)
      _ = Thread.sleep(10000)
      listing <- listings
      e = s.getEnhancedListing(listing)(key)
      csv = new CsvWrapper(e).toCsv()
      _ = pw.write(csv + "\n")
      _ = println(csv)
    }

    pw.close()
  }
}
