package u05lab.ex2

import org.junit.Assert.assertEquals
import org.junit.Test

class TestConferenceReviewing:
  var cr = ConferenceReviewing()
  cr.loadReview(1, 8, 8, 6, 8)
  cr.loadReview(1, 9, 9, 6, 9) // 5.4
  cr.loadReview(2, 9, 9, 10, 9) // 9.0
  cr.loadReview(2, 4, 6, 10, 6) // 6.0
  cr.loadReview(3, 3, 3, 3, 3) // 0.9
  cr.loadReview(3, 4, 4, 4, 4) // 1.6
  cr.loadReview(4, 6, 6, 6, 6) // 3.6
  cr.loadReview(4, 7, 7, 8, 7) // 5.6
  var map: Map[Question, Int] = Map()
  map = map + (Question.Relevance() -> 8)
  map = map + (Question.Significance() -> 8)
  map = map + (Question.Confidence() -> 7)
  map = map + (Question.Final() -> 8)
  cr.loadReview(4, map)
  cr.loadReview(5, 6, 6, 6, 10) // 6.0
  cr.loadReview(5, 7, 7, 7, 10) // 7.0

  @Test def testOrderedScores() =
    assertEquals(cr.orderedScores(2, Question.Relevance()),List(4,9))
    assertEquals(cr.orderedScores(4, Question.Confidence()), List(6, 7, 8))
    assertEquals(cr.orderedScores(5, Question.Final()), List(10, 10))

  @Test def testAverageFinalScore() =
    assertEquals(cr.averageFinalScore(1), 8.5, 0.01)
    assertEquals(cr.averageFinalScore(2), 7.5, 0.01)
    assertEquals(cr.averageFinalScore(3), 3.5, 0.01)
    assertEquals(cr.averageFinalScore(4), 7.0, 0.01)
    assertEquals(cr.averageFinalScore(5), 10.0, 0.01)

  @Test def testAcceptedArticles() =
    assertEquals(cr.acceptedArticles(), Set(1,2,4));

  @Test def testSortedAcceptedArticles() =
    assertEquals(cr.sortedAcceptedArticles(), List((4,7.0),(2,7.5),(1,8.5)));
