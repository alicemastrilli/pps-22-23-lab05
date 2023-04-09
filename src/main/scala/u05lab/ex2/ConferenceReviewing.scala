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
  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = new ConferenceReviewingImpl

class ConferenceReviewingImpl extends ConferenceReviewing:
  private var reviews: List[(Int, Map[Question, Int])] = List()

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    reviews = (article, scores) :: reviews

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    var map: Map[Question, Int] = Map()
    map = map + (Question.Relevance() -> relevance)
    map = map + (Question.Significance() -> significance)
    map = map + (Question.Confidence() -> confidence)
    map = map + (Question.Final() -> fin)
    reviews = (article, map) :: reviews


  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews.filter(p => p._1.equals(article)).map(p => p._2.apply(question)).sorted

  override def averageFinalScore(article: Int): Double =
    reviews.filter(p => p._1.equals(article)).map(p => p._2.apply(Question.Final())).sum.toDouble /
      reviews.filter(p => p._1.equals(article)).map(p => p._2.apply(Question.Final())).length

  override def acceptedArticles(): Set[Int] = ???

  override def averageWeightedFinalScoreMap(): Map[Int, Double] = ???
