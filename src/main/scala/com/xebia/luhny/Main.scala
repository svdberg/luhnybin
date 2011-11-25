package com.xebia.luhni

import io.Source

object Main {
    def main(args: Array[String]) {
      Iterator.continually(Console.readLine).takeWhile(t => t != null && t != "").foreach(line => println( maskLuhn(line.toList)))
    }

    def recursiveReplace(i: Int, list: List[Char]): List[Char] = list match {
        case head :: tail => {
             if (head.isDigit) {
                val m = math.max(i,luhnLength(list)) //calc the longest matching sequence
                val res = if ( m > 0 ) 'X' else head
                res :: recursiveReplace(m - 1, tail)
            } else //this is a non CC token
                head :: recursiveReplace(i, tail) //meh not tail recursive
        }
        case nil => List.empty
    }

    def luhnLength(list: List[Char]): Int = List.range(14, 17).map(x => dluhn(list.filter(_.isDigit), x)).max
    def dluhn(list: List[Char], len: Int): Int = {
      val es = list.take(len)
      if (es.length == len && luhn((len+1), es)) len else 0 //trick clause on the if: length of take may be smaller!
    }

    def luhn(i: Int,  list: List[Char]) = (numbers(i), list).zipped.map((x,y) => calc(x, y)).sum % 10 == 0
    def numbers(i:Int):Stream[Int] = i #:: numbers(i+1) //endless list of ints
    def divmod(x: Int, y: Int): (Int, Int) = (x / y, x % y)

    def calc(i: Int, x: Char) = {
      val tup = divmod(((1 + i % 2) * x.getNumericValue) , 10)
      tup._1 + tup._2
    }

    def groupByS(eq: (Char,Char) => Boolean, list: List[Char]): List[List[Char]] = list match {
        case head :: tail => {
          val newHead = head :: tail.takeWhile(eq(head,_))
          newHead :: groupByS(eq, tail.dropWhile(eq(head,_)))
        }
        case nil => List.empty
    }

    def split(s: List[Char]): List[List[Char]] = groupByS((a,b) => isDigit(a) == isDigit(b), s)
    def maskLuhn(line: List[Char]) = (for (token <- split(line)) yield recursiveReplace(0,token).mkString).mkString
    def isDigit(input: Char) = "0123456789- ".contains(input)
}