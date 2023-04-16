package u05lab.ex2

import scala.collection.mutable

enum Question:
  case Relevance()
  case Significance()
  case Confidence()
  case Final()
trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int,
                 confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[(Int, Double)]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = new ConferenceReviewingImpl

class ConferenceReviewingImpl extends ConferenceReviewing:
  import Question.*
  private var reviews: List[(Int, Map[Question, Int])] = List()

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    reviews = (article, scores) :: reviews

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    var map: Map[Question, Int] = Map()
    map = map + (Relevance() -> relevance)
    map = map + (Significance() -> significance)
    map = map + (Confidence() -> confidence)
    map = map + (Final() -> fin)
    reviews = (article, map) :: reviews


  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews.filter(p => p._1 == article).map(p => p._2(question)).sorted

  private def computeAverage(l: List[Double]): Double =
    l.sum / l.length
  override def averageFinalScore(article: Int): Double =
    computeAverage(reviews.filter(p => p._1 == article).map(p => p._2(Final())))

  override def acceptedArticles(): Set[Int] =
    reviews.filter(a => averageFinalScore(a._1) > 5).filter(a => a._2(Relevance()) >= 8).
      map(a => a._1).toSet

  override def sortedAcceptedArticles(): List[(Int, Double)] =
    acceptedArticles().map(a => (a, averageFinalScore(a))).toList.sortBy(a => (a._2, a._1))


  override def averageWeightedFinalScoreMap(): Map[Int, Double] =
    reviews.map(a => (a._1, computeAverage(reviews.filter(b => b._1 == a._1).map(c => (c._2(Confidence()).
      toDouble * c._2(Final()).toDouble)/10)))).toMap

