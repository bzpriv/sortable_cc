package main

import argonaut._, Argonaut._, ArgonautShapeless._
import better.files._
import scalaz.\/
import scalaz.std.list._
import scalaz.std.map._
import scalaz.std.tuple._
import scalaz.syntax.functor._
import scalaz.syntax.id._
import scalaz.syntax.semigroup._
import scalaz.syntax.std.either._
import scalaz.syntax.std.list._
import shapeless.{lens => shapelessLens}

object Data {

  case class Indexed[A](
    item: A, category: String, cost: Double,
    identifier: String)

  implicit class IndexOps[A](a: A)(implicit hi: HasIndex[A]) {
    def index = hi.index(a)
  }

  trait HasIndex[A] {
    def index(a: A): Indexed[A]
  }

  case class Product(
    product_name: String, manufacturer: String,
    family: Option[String], model: String,
    `announced-date`: String)

  val decodeProduct = DecodeJson.of[Product]

  def stripDupSpace(s: String) =
    "\\s+".r.replaceAllIn(s, " ")

  // Some product names result in "protected" terms, which would create too many
  // false positives
  // TODO - investigate more sophisticated solution that filters out fewer legit matches
  val protectedTerms = Set("zoom")

  implicit val indexProduct: HasIndex[Product] =
    new HasIndex[Product] {
      def index(product: Product) =
        Indexed(product,
          product.manufacturer.toLowerCase, 0.0,
          (product.product_name.toLowerCase.replace("_", " ") |>
          { name =>
              Some(s"(\\W|\\b)${product.manufacturer.toLowerCase}(\\W|\\b)".r.replaceAllIn(name, "") |>
                  // excluding family dramatically increases hit rate
                  { id => product.family.map(f => id.replace(f.toLowerCase, ""))
                            .getOrElse(id) })
                .filterNot(x => protectedTerms.contains(x.trim))
                .getOrElse(name)
           }) |> { id => stripDupSpace(" " + id.trim + " ") })
     }

  // Some manufacturers are known aliases for others,
  // we can improve match rates by mapping to a canonical version
  val manufacturerMappings = Map(
    "hewlett packard" -> "hp",
    "fuji photo film europe gmbh" -> "fujifilm")

  case class Listing(
    title: String, manufacturer: String,
    currency: Currency, price: Double)

  type ProductMap = Map[Indexed[Product], List[Indexed[Listing]]]

  object Listing {
    type Folder = (Job, Indexed[Listing]) => Job
  }

  val decodeListing = DecodeJson.of[Listing]

  sealed trait Currency
  case object CAD extends Currency
  case object USD extends Currency
  case object EUR extends Currency
  case object GBP extends Currency

  implicit val codecCurrency: CodecJson[Currency] =
    CodecJson({
      case CAD => "CAD".asJson
      case USD => "USD".asJson
      case EUR => "EUR".asJson
      case GBP => "GBP".asJson
    }, c => c.focus.string match {
      case Some("CAD") => DecodeResult.ok(CAD: Currency)
      case Some("USD") => DecodeResult.ok(USD: Currency)
      case Some("EUR") => DecodeResult.ok(EUR: Currency)
      case Some("GBP") => DecodeResult.ok(GBP: Currency)
      case _ => DecodeResult.fail("Could not decode currency", c.history)
    })


  def normalizeCost(listing: Listing) = listing.currency match {
    case CAD => listing.price
    case USD => listing.price * 1.36
    case EUR => listing.price * 1.47
    case GBP => listing.price * 1.73
  }

  implicit val indexListing: HasIndex[Listing] =
    new HasIndex[Listing] {
      def index(listing: Listing) =
        Indexed(listing,
          (listing.manufacturer.toLowerCase |>
            { m => manufacturerMappings.get(m).getOrElse(m) }),
          normalizeCost(listing),
          // padding with exactly one space allows use of
          // whitespace as delimiter regardless of string boundary
          // for matching in hot loop without regexp
          stripDupSpace((" " + listing.title.toLowerCase + " ").trim))
    }

  case class Result(
    product_name: String,
    listings: List[Listing])

  case class MissedMatch(category: String, missed: List[String],
                         available: List[String],
                         families: List[String])

  implicit val encodeMiss = EncodeJson.of[List[MissedMatch]]

  case class DupeMatch(category: String, listing: String, matched: List[Indexed[Product]])
  case class DupeCategoryMatch(category: String, matched: List[String])

  implicit val encodeIxProduct = EncodeJson.of[List[Indexed[Product]]]
  implicit val encodeDupe = EncodeJson.of[List[DupeMatch]]
  implicit val encodeDupeCat = EncodeJson.of[List[DupeCategoryMatch]]

  case class Job(
    noMatchManufacturer: List[Indexed[Listing]],
    multipleMatchManufacturer: List[DupeCategoryMatch],
    noMatchProduct: List[Indexed[Listing]],
    multipleMatchProduct: List[DupeMatch],
    resultMap: ProductMap) {
      lazy val resultCount = results.foldLeft(0)(_ + _.listings.size)
      lazy val results = resultMap.toList.map(t => Result(t._1.item.product_name, t._2.map(_.item)))

      def missedMatches(products: Map[String, List[Indexed[Product]]]) =
        noMatchProduct.groupBy(_.category).map(x =>
          products.get(x._1).getOrElse(List()) |> { inCat =>
            MissedMatch(x._1, x._2.map(_.identifier),
              inCat.map(_.identifier).sorted, inCat.flatMap(_.item.family))}).toList
   }

  object Job {
    val empty = Job(List(), List(), List(), List(), Map())

    val lens = shapelessLens[Job]

    def formatDebug[A](a: A)(implicit enc: EncodeJson[A]) = enc(a).spaces2
  }

  implicit val encodeResult = EncodeJson.of[Result]
}

object Solution extends App {
  import Data._

  implicit class TapOps[A](a: A) {
    def tap[B](f: A => B) = {
      f(a)
      a
    }
  }

  def readFile[A](fileName: String, parse: String => \/[String, A]):
  (List[(String, String)], List[A]) =
    scala.io.Source.fromFile(fileName) |> { source =>
      \/.fromTryCatchNonFatal(source.getLines.toList)
        .map(_.tap(_ => source.close))
        .leftMap(_.tap(_ => source.close))
        .fold(
          _ => (List((s"failed reading ${fileName}", "")), List()),
          _.foldLeft((List[(String, String)](), List[A]()))((acc, line) =>
            parse(line).fold(
              error => (acc._1 :+ (error, line), acc._2),
              a => (acc._1, acc._2 :+ a))))
    }

  def writeFile(fileName: String, lines: List[String]) =
    \/.fromTryCatchNonFatal(File(fileName).overwrite(lines.mkString("\n")))

  lazy val parseProducts =
    readFile("./products.txt",
        _.decodeEither[Product]
         .disjunction.map(_.index))
      .map(_.groupBy(_.category).mapValues(
        // Some products are actually duplicated, eg Samsung-SL202 and Samsung_SL202
        _.groupBy(_.identifier).map(_._2.headOption.toList).flatten.toList))

  lazy val parseListings =
    readFile("./listings.txt",
        _.decodeEither[Listing]
         .disjunction.map(_.index))

  // use surrounding spaces as boundaries to avoid regex cost in hot loop
  // trim and re-add to ensure exactly one space at beginning and end
  def matches(s1: String, s2: String) = {
    val (lg, sm) =
      Some(s1).filter(_.length > s2.length).map(_ => (s1, s2)).getOrElse((s2, s1))
    s" ${lg.trim} ".contains(s" ${sm.trim} ") ||
    // has extraneous hyphen
    s" ${lg.trim} ".contains(s" ${sm.trim.replace("-", "")} ") ||
    // has extraneous spaces
    s" ${lg.trim} ".contains(s" ${sm.trim.replace(" ", "")} ") ||
    // has hyphen in place of space
    s" ${lg.trim} ".contains(s" ${sm.trim.replace("-", " ")} ") ||
    // missing hyphen
    s" ${lg.trim.replace("-", "")} ".contains(s" ${sm.trim} ")
  }

  def filterProduct(products: Map[String, List[Indexed[Product]]]): Listing.Folder =
    (job: Job, listing: Indexed[Listing]) =>
      products.get(listing.category)
        .toList.flatten
        .filter(p => matches(p.identifier, listing.identifier)) match {
          case head :: Nil =>
            Job.lens.resultMap.modify(job)(_ |+| Map(head -> List(listing)))
          case head :: tail =>
            Job.lens.multipleMatchProduct.modify(job)(
              _ :+ DupeMatch(listing.category, listing.identifier, head :: tail))
          case Nil => Job.lens.noMatchProduct.modify(job)(_ :+ listing)
        }

  def filterCategory(categories: Set[String], filterProduct: Listing.Folder): Listing.Folder =
    { (job: Job, listing: Indexed[Listing]) =>
        categories.filter(matches(_, listing.category)).toList match {
          case head :: Nil => filterProduct(job, listing.copy(category = head))
          case head :: tail => Job.lens.multipleMatchManufacturer.modify(job)(
                                _ :+ DupeCategoryMatch(listing.category, head :: tail))
          case _ => Job.lens.noMatchManufacturer.modify(job)(_ :+ listing)
        }
     }

  lazy val result = {
    println("Reading data")
    val (productErrors, products) = parseProducts
    productErrors.toNel.foreach(println(_))
    val (listingErrors, listings) = parseListings
    listingErrors.toNel.foreach(println(_))
    println("Processing data")
    val start = System.currentTimeMillis
    val job = listings.foldLeft(Job.empty)(filterCategory(products.keys.toSet, filterProduct(products)))
    val end = System.currentTimeMillis
    println(s"Completed in ${(end - start)/1000} seconds")
    println(s"Rejected ${job.noMatchManufacturer.size} listings with no matching manufacturer")
    println(s"Rejected ${job.multipleMatchManufacturer.size} listings with multiple matching manufacturers")
    println(s"Rejected ${job.noMatchProduct.size} listings by product identifier")
    println(s"Rejected ${job.multipleMatchProduct.size} listings with multiple matches")
    println(s"Matched ${job.resultCount} of ${parseListings._2.size} listings (${
                ((job.resultCount.toDouble/parseListings._2.size.toDouble)*100.0).toInt}%)")
    writeFile("results.txt", job.results.map(encodeResult(_).nospaces))
    println("Results printed to ./results.txt")
    job
  }

  lazy val missedMatches = result.missedMatches(parseProducts._2)
  lazy val dupeMatches = result.multipleMatchProduct
  lazy val priceRange = (ls: List[Listing]) => ls.map(normalizeCost) |> { prices => (prices.min / prices.max) }
  lazy val priceRanges =
    result.results.filter(_.listings.map(normalizeCost(_)).toSet.size > 1).map(r =>
        (priceRange(r.listings), r.listings.sortBy(normalizeCost(_)).map(x => (x.title, normalizeCost(x)))))
      .sortBy(_._1)

  result
}
