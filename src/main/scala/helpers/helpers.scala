package helpers

import scala.annotation.tailrec

//  Aux function for converting a list to string
//  Takes each word recursively and concats to the final result
def listToString(l: List[String], c: Char): String = {
  l match {
    case head :: tail =>
      tail match {
        case Nil => head + listToString(tail, c)
        case _ => head + c + listToString(tail, c)
      }
    case Nil => ""
  }
}

//  Aux function for iterating through columns
//  Returns the wanted column or Nil
@tailrec
def iterateCols(headers: List[String], tMat: List[List[String]])
               (col: String): List[String] = {
  headers match {
    case Nil => Nil
    case head :: tail =>
      if (head.equals(col)) head :: tMat.head
      else iterateCols(tail, tMat.tail)(col)
  }
}