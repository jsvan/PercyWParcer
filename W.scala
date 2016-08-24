package com.vanecek.perce.nlNetwork

import com.vanecek.perce.util.{EndLoopException, Logger}

/**
 * Created by julian on 5/31/15.
 */
/**
 * Word is a single word, number or punctuation.
 *
 * @param wText the word text with no white space and lower cased.
 */
class W(val wText:String) extends Logger("W") {
  require(!wText.contains(' '),
          "Word '"+wText+"' must have no white spaces.")
  if(!wText.forall(c => c.isLower || NLSentences.isCEndOfSentence(c) || c == '\''))
    warn("Word '"+wText+"' is malformed.")
  override def toString = wText
}

object W extends Dictionary {
  val logger = Logger("obj W")

  private def addWord(text: String): W = {
    assert(!text.contains(' '))
    val wText = text.toLowerCase()
    val fWord = if(dictionary.contains(wText)) {
      logger.trace("Retrieved word "+wText)
      dictionary.get(wText).get
    } else {
      logger.trace("Inserted word "+wText)
      val fw = FWord(new W(wText))
      dictionary += (wText -> fw)
      fw
    }
    fWord.count += 1  // Count the use of this word in a sentence
    fWord.word
  }

  def apply(wText:String): W = addWord(wText)

  def unapply(w:W) = Option(w.wText)

  /**
   * Convert a sentence string to an array of Word's and expanded contractions.
   *
   * @param text The String containing the sentence to be split into Word's.
   * @return the array of words.
   */
  def words(text: String): Array[W] =
    // TODO: Julian after the filter, hook in the ContractionExpander's removeContraction method before the map...
    text.split("(?=[ ,!?.;:])|(?<=[ ,!?.;:])")
      .filter( _ != " ")
      .map(addWord(_))

  def forEachWord(f:(Int,FWord) => Unit): Unit =
    try {
      var i = 0
      dictionary.toSeq.sortBy(_._1).foreach(sfw => {
        f(i, sfw._2)
        i += 1
      })
    } catch {
      case _:EndLoopException =>
    }
}

//--------------------------------------------------------
/**
 * The Fragment Handler manages the Dictionary Fragments.
  */
object FragHandler extends Logger("FragHandler") {

  def addFragment(words: Array[W]): Unit = {
    trace("Adding Fragment: " + words.map(_.wText).mkString(" "))

    var dict: Dictionary = W                          //start at main dictionary
    var thisFW = dict.getDicFWord(words(0)) match {   //get the first fWord
      case Some(fWord) =>                             //first fWord exists
        fWord
      case None =>                                    //first fWord does not exist in main dictionary
        dict.addWord(words(0))                        //add that sucker to main dictionary and return
    }

    for(i <-1 to words.length-1)                     //start at 1 because already did first iteration
      thisFW.nextWord.fold({dict=new Dictionary; thisFW.nextWord=Option(dict);thisFW=dict.addWord(words(i))}) {dict=_; thisFW=dict.getDicFWord(words(i)).fold( dict.addWord(words(i))){_}}                         //look to its next dictionary
       /* case Some(nDict) =>                           //dictionary exists
          dict = nDict
          thisFW = dict.getDicFWord(words(i)).fold( dict.addWord(words(i))){_}
        /*   {//get the next FWord
             case Some(fWord) =>                       //okaydokay
 fWord
             case None =>                              //word doesnt exist, add it
 dict.addWord(words(i))
          }
       */  case None =>                                  //dictionary does not exist
          dict = new Dictionary
          thisFW.nextWord = Option(dict)
          thisFW = dict.addWord(words(i))             //make new dictionary, add the word to it, return new word.
      }*/
    thisFW.count +=1                                 //now increment final fWord's count
    trace("Final count on "+thisFW.word.wText+": "+thisFW.count)
  }

  /*
  def printFragments(dict: Dictionary, level: Int): Unit = {//TODO this doesn't work right
   dict.dictionary.foreach(fWord=> {
     val nDict = fWord._2.getNextWord match {
       case Some(dic)=> {

         println("Dictionary # is => "+level + " ::: " + fWord._2.toString + " :: " + fWord._2.count)
         printFragments(dic, level+1)
     } case None => {println("Dictionary # is => "+level + " ::: " + fWord._2.toString + " :: " + fWord._2.count)}
     }
     })
   }
   */

  /**
   * Apply a function f to each fragment (collected as a List[FWords]).
   *
   * @param f the function to apply. Takes two arguments the counter and the list of FWords in the fragment.
   */
  def forEachFragment(f: (Int, List[FWord]) => Unit): Unit = {
    var index = 0

    def g(dict:Dictionary, fWords:List[FWord]):Unit = {
      dict.dictionary.foreach(sfw => {
        val fWords2 = fWords :+ sfw._2
        if(sfw._2.nextWord.isDefined)
          g(sfw._2.nextWord.get, fWords2)
        else if(fWords2.length > 0) {          // take only fragments with 2 or more words
          index += 1
          f(index, fWords2)
        }
      })
    }

    try {
      g(W, List.empty[FWord])
    } catch {
      case _:EndLoopException =>
    }
  }
}
