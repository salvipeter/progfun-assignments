package forcomp
import common._

object Anagrams {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  // ex. 1a
  def wordOccurrences(w: Word): Occurrences = {
    w.toLowerCase.groupBy(identity).map((c) => (c._1, c._2.length)).toList.sortBy(_._1)
  }

  // ex. 1b
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.foldRight("")(_++_))

  // ex. 2a
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.groupBy(wordOccurrences)
  }

  // ex. 2b
  def anagramsByOccurrences(occurrences: Occurrences): List[Word] = {
    val all = dictionaryByOccurrences.get(occurrences)
    if (all.isEmpty) Nil else all.get
  }
  def wordAnagrams(word: Word): List[Word] = anagramsByOccurrences(wordOccurrences(word))

  // ex. 3
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    if (occurrences.isEmpty)
      List(Nil)
    else {
      val next = occurrences.head
      val rest = combinations(occurrences.tail)
      val result = for (i <- 0 to next._2; subset <- rest) yield {
        if (i == 0) subset else (next._1, i) :: subset
      }
      result.toList
    }
  }

  // ex. 4a
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def updateMap(a: Map[Char, Int], b: (Char, Int)) = {
      val n = a.get(b._1).get // we know that this exists
      if (n == b._2)
        a - b._1
      else
        a.updated(b._1, n - b._2)
    }
    y.foldLeft(x.toMap)(updateMap).toList.sortBy(_._1)
  }

  // ex. 4b
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def rec(occurrences: Occurrences): List[Sentence] = {
      if (occurrences.isEmpty)
        List(Nil)
      else {
        for (comb <- combinations(occurrences);
             word <- anagramsByOccurrences(comb);
             sentence <- rec(subtract(occurrences, comb)))
        yield word :: sentence
      }
    }
    rec(sentenceOccurrences(sentence))
  }

}
