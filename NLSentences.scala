package com.vanecek.perce.nlNetwork

import com.vanecek.perce.util.History

/**
 * Created by julian on 5/31/15.
 */
//---------------------------------------------------
/**
 * The NLSentences Node contains all the sentences, in time entered order.
 */
class  NLSentences extends History[S](50) {
  override def add(s:S): Unit = {
    findFragments(s).foreach(FragHandler.addFragment(_))
    super.add(s)
  }
  def add(words: Array[W]): Unit = add(S(words))
  def clear: Unit = clear(resetValue = S.emptyS)

  /**
   * Compare the new sentence to all existing sentences in the history and find all new unique fragments.
   * A fragment is two or more words that do not contain end-of-sentence punctuations, like the period.
   *
   * @param newS the new sentence (not in history yet).
   * @return the list of unique fragments
   */
  def findFragments(newS:S): List[Array[W]] = {
    var fragments = List.empty[Array[W]]
    val newSWords = newS.words
    foreach((_,s) => {
      for(newSI <- 0 to newSWords.length - 2) {
        val sWords = s.words
        for (sI <- 0 to sWords.length - 2) {
          var len = 0
          while(newSI+len < newSWords.length &&
            sI+len < sWords.length &&
            newSWords(newSI+len).wText == sWords(sI+len).wText &&
            !NLSentences.isSEndOfSentence(sWords(sI+len).wText))
            len += 1
          if(len > 1) {
            val frag = sWords.slice(sI, sI + len)
            // Insert the fragment if its unique.  Do not take duplicates.
            if(fragments.forall(f => f.deep != frag.deep ))
              fragments = frag +: fragments
          }
        }
      }
    })
    fragments
  }
}

object NLSentences {
  def apply() = new NLSentences()

  def isCEndOfSentence(c: Char) =
    c == '.' || c == '?' || c == '!' || c == ';' || c == ':'

  def isSEndOfSentence(s: String) =
    s.length == 1 && isCEndOfSentence(s(0))
}

//---------------------------------------------------
/**
 * Sentence contains fragments or words.
 *
 * @param words
 */
class S(val words: Array[W]) {
  override def toString = words.foldLeft("")((s,word) => s+(if(s=="(") "" else " ")+word.wText)
}

object S {
  val emptyS = new S(Array.empty[W])
  def apply(words: Array[W]) = {
    require(words.length > 1, "A sentence cannot be empty.")
    new S(words)
  }
  def unapply(s:S) = Option(s.words)
}

