package com.vanecek.perce.nlNetwork

import scala.collection.mutable

/**
 * This the Perce Dictionary keeps a set of unique Words.
 *
 * Created by julian on 2/19/15.
 */
class Dictionary {
  /*
   * The dictionary of unique words.
   * Maps a String to FWord
   */
   val dictionary = mutable.Map[String, FWord]()

  /**
   * Add the given word to the dictionary and its word fragment. If a word fragment already exists with that word,
   * the one in the dictionary is returned otherwise a new fragment word is created and returned.
   *
   * @param w the new word to add to the dictionary
   * @return the FWord associated with the unique word
   */
  def addWord(w: W): FWord = {
    if(!dictionary.contains(w.wText))
      dictionary += (w.wText -> FWord(w))
    dictionary.get(w.wText).get
  }

  def print: Unit = {
    dictionary.foreach((fWord) => println(FWord + ", "))
  }
  /**
   * Get the Dictionary Fragment statistics.
   * This includes determining the longest fragment and the number of fragments.
   *
   * @return the pair (max fragment length, number of fragments)
   */
  def stats(): (/*max depth*/Int, /*fragments*/Int) =
    dictionary.foldLeft((0,0))((result,sfw) => {
      val fw = sfw._2
      val (d,n) =
        if(fw.nextWord.isDefined)
          fw.nextWord.get.stats
        else
          (0,0)
      (Math.max(result._1,d+1), result._2+n+fw.count)})

  /**
   * Clear out the dictionary. Add entries are removed.
   */
  def clearDictionary(): Unit =
    dictionary.clear()

  def getDicFWord(word: W): Option[FWord] = dictionary.get(word.wText)
  def size: Int = dictionary.size
}

//----------------------------------------------
/**
 * Fragment Word is a node of the Fragment Tree.
 * It is a container to track a word inside a Fragment Word tree.
 *
 * @param word A word
 * @param count The count of seen sentences that contain the word fragment ending with this word.
 * @param nextWord The next word(s) in the fragment tree.
 */
class FWord(val word: W,
            var count: Int,
            var nextWord: Option[Dictionary] = None) {
  override def toString: String ={
    word.toString
  }
}

object FWord {
  def apply(word:W) = new FWord(word, 0, None)
}
