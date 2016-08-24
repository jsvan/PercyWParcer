package com.vanecek.perce.nlNetwork

/**
 * List of English contractions. These are converted to their expanded non-contraction phrase.
 *
 * @todo deal with tense. Currently only the present tense is recognized. So, She's is mapped
 *       to "she is", but it could be "she has".
 *
 * Created by george on 3/9/15.
 */
object ContractionExpander {

  private val overloadedContractions = Map (
    "ain't"   -> (List("am", "not"),     List("is","not")),
    "aren't"  -> (List("are","not"),     List("am","not")), // as in "Aren't I great?"
    "he'd"    -> (List("he","would"),    List("he","had")),
    "he's"    -> (List("he","is"),       List("he","has")),
    "how'd"   -> (List("how","would"),   List("how","did")),
    "how's"   -> (List("how","is"),      List("how","has"),     List("how","does")),
    "i'd"     -> (List("i","would"),     List("i","had")),
    "it'd"    -> (List("it","would"),    List("it","had")),
    "it'll"   -> (List("it","will"),     List("it","shall")),
    "it's"    -> (List("it","is"),       List("it","has")),
    "she'd"   -> (List("she","would"),   List("she","had")),
    "that's"  -> (List("that","is"),     List("that","has")),
    "there'd" -> (List("there","would"), List("there","had")),
    "there's" -> (List("there","is"),    List("there","has")),
    "they'd"  -> (List("they","would"),  List("they","had")),
    "we'd"    -> (List("we","would"),    List("we","had")),
    "what's"  -> (List("what","is"),     List("has","has"),     List("what","does")),
    "when's"  -> (List("when","is"),     List("when","has")),
    "wheren's" -> (List("wheren","is"),  List("wheren","has")),
    "who'd"   -> (List("who","would"),   List("who","had")),
    "who'll"  -> (List("who","will"),    List("who","shall")),
    "who's"   -> (List("who","is"),      List("who","has")),
    "why's"   -> (List("why","is"),      List("why","has")),
    "you'd"   -> (List("you","would"),   List("you","had"))
  )

  private val contractions = Map(
    "can't"    -> List("cannot"),
    "could've" -> List("could","have"),
    "couldn't" -> List("could","not"),
    "didn't"   -> List("did","not"),
    "doesn't"  -> List("does","not"),
    "don't"    -> List("do","not"),
    "hadn't"   -> List("had","not"),
    "hasn't"   -> List("has","not"),
    "haven't"  -> List("have","not"),
    "he'll"    -> List("he","will"),
    "how'll"   -> List("how","will"),
    "i'll"     -> List("i","will"),
    "i'm"      -> List("i","am"),
    "i've"     -> List("i","have"),
    "isn't"    -> List("is","not"),
    "let's"    -> List("let","us"),
    "ma'am"    -> List("madam"),
    "mightn't" -> List("might","not"),
    "might've" -> List("might","have"),
    "mustn't"  -> List("must","not"),
    "must've"  -> List("must","have"),
    "needn't"  -> List("need","not"),
    "o'clock"  -> List("of","the","clock"),
    "oughtn't" -> List("ought","not"),
    "shan't"   -> List("shall","not"),
    "she'll"   -> List("she","will"),
    "she's"    -> List("she","is"),
    "should've"-> List("should","have"),
    "shouldn't"-> List("should","not"),
    "that'll"  -> List("that","will"),
    "there're" -> List("there","are"),
    "they'll"  -> List("they","will"),
    "they're"  -> List("they","are"),
    "they've"  -> List("they","have"),
    "wasn't"   -> List("was","not"),
    "we'll"    -> List("we","will"),
    "we're"    -> List("we","are"),
    "we've"    -> List("we","have"),
    "weren't"  -> List("were","not"),
    "what'll"  -> List("what","will"),
    "what're"  -> List("what","are"),
    "what've"  -> List("what","have"),
    "wheren'd"  -> List("wheren","did"),
    "wheren've" -> List("wheren","have"),
    "who're"   -> List("who","are"),
    "who've"   -> List("who","have"),
    "why'll"   -> List("why","will"),
    "why're"   -> List("why","are"),
    "won't"    -> List("will","not"),
    "would've" -> List("would","have"),
    "y'all"    -> List("you","all"),
    "you'll"   -> List("you","will"),
    "you're"   -> List("you","are"),
    "you've"   -> List("you","have")
  )

  /**
   * Flatten a nested list of strings, as in
   * {{{
   * flattenIt(List("a",List("b","c"))) == List("a","b","c")
   * }}}
   *
   * @param list the nested list to flatten
   * @return the flattened list of strings
   */
  private def flattenIt(list: List[Any]): List[String] = list flatten {
    case s: List[Any] => flattenIt(s)
    case e: String => List(e)
  }

  /**
   * Given a list of words, replace any one contraction with its expanded form.  This method does not replace
   * overloaded contractions such as "he's", as without context it is an ambiguous contraction.  It could be "he is" or
   *
   * @param words The list of words with contractions.
   * @param insertSpaces optional Boolean flag to insert a space between words of the contraction.
   * @return The list of words with contractions expanded.
   */
  def removeContractions(words:List[String], insertSpaces: Boolean = false): List[String] = //TODO change from inpassing list to array.
    flattenIt(words.map(w => { // todo rewrite without the use of get. See [[http://blog.originate.com/blog/2014/06/15/idiomatic-scala-your-options-do-not-match/]]
      if(contractions.contains(w)) {
        val ws = contractions.get(w.toLowerCase).get
        ws.length match {
          case 1 => ws(0)
          case 2 => if(insertSpaces) List(ws(0), " ", ws(1)) else ws
        }
      } else
        w
    }))

  /**
   *
   * @param sentence  A sentence with possible contractions.
   * @return The sentence with contractions expanded.
   */
  def removeContractions(sentence: String): String =
    removeContractions(sentence.split("(?=[ ,!?.])|(?<=[ ,!?.])").toList, insertSpaces = true).mkString
}
