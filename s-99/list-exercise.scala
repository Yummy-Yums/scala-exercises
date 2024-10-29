

object Main extends App {


  val e = List(1,2,3,4,5)

  // find the last element of a list
  val lastElement = e match {
    case last :: Nil => last
    case _ :: y => y.last
    case Nil => throw new NoSuchElementException("List is empty")
  }

  // find the last but one element

  val lastButOneElem = e match {
    case penultimate :: _ :: Nil => penultimate
    case _ :: tail => tail.init.last 
    case _ => throw new NoSuchElementException("List is empty")
  }

  // find the Kth element of a list

  def findKthElement[A](kth: Int, list: List[A]): A = {
    var num = 0
    var lis = list
    
    while(lis.nonEmpty){
      if (num == kth) return lis.head
      lis = lis.tail
      num += 1
    }

    throw new NoSuchElementException("Index out of bounds")
    
  }

  def isPalindrome[A](list: List[A]): Boolean = {
    var second = List.empty[A]

    var temp = list

    while(temp.nonEmpty){
      second = temp.head :: second
      temp = temp.tail

    }

    return list == second
  }

  def isPalindrome2[A](list: List[A]): Boolean = {
  val len = list.length
  var firstHalf = list.take(len / 2)
  var secondHalf = list.takeRight(len / 2).reverse

  while (firstHalf.nonEmpty && secondHalf.nonEmpty) {
    if (firstHalf.head != secondHalf.head) {
      return false
    }
    firstHalf = firstHalf.tail
    secondHalf = secondHalf.tail
  }

  true
}

  def flatten(list: List[Any]): List[Any] = {
    
    list match {
      case Nil => Nil
      case (x: List[Any]) :: xs => flatten(x) ::: flatten(xs)
      case x :: xs =>  x :: flatten(xs)
    }

  }

  def compressRecursive[A](ls: List[A]): List[A] = ls match {
    case Nil       => Nil
    case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
  }


  def pack[A](ls: List[A]): List[List[A]] = {

    //ls.groupBy(identity).values.toList
    ls match {
      case Nil => Nil
      case h :: tail =>
        val (first, rest) = ls.span(_ == h)
        first :: pack(rest)
    }
  
  }

  def encode[A](ls: List[A]): List[(Int, A)] = {

   var res = List.empty[(Int, A)]
  
   pack(ls).foreach(xs =>
     res = (xs.size, xs.head) :: res
   )

   res
  }

  def encodeModified[A](ls: List[A]): List[Any] = {

   var res = List.empty[Any]
  
   pack(ls).foreach(xs =>
     if (xs.size != 1){
       res = (xs.size, xs.head) :: res
     } else {
       res = xs.head :: res
     }
   )

   res
  }

  def decode[A](xs: List[(Int, A)]): List[A] = {
     xs.flatMap { elem =>
       List.fill(elem._1)(elem._2)
     }
  }

  def duplicate[A](xs: List[A]): List[A] = {
     xs.flatMap { elem =>
       List.fill(2)(elem)
     }
  }

  def duplicateN[A](number: Int, xs: List[A]): List[A] = {
     xs.flatMap { elem =>
       List.fill(number)(elem)
     }
  }

  def split[A](length: Int, xs: List[A]): Tuple2[List[A], List[A]] = {
    xs.splitAt(length)
  }

  def rotate[A](size: Int, xs: List[A]): List[A] = {
    size match {
      case size if size == 0 => xs
      case size if size > 0 => xs.drop(size) ++ xs.take(size)
      case size if size < 0 => {xs.takeRight(scala.math.abs(size)) ++ xs.dropRight(scala.math.abs(size))} 
    }
  }

   def removeAt[A](index: Int, xs: List[A]): (List[A], A) = {
    xs.splitAt(index) match {
      case (n, xs) if index < 0 => throw new NoSuchElementException
      case (pre, e :: post) => (pre ::: post, e)
      case (Nil, _) => throw new NoSuchElementException
    }
  }

   def insertAt[A](element: A, index: Int, xs: List[A]): List[A] = {
    xs.splitAt(index) match {
      case (n, xs) if index < 0 => throw new NoSuchElementException
      case (pre, post) => pre ::: element :: post
      case (Nil, _) => throw new NoSuchElementException
    }
  }

  def rangeMethod(start: Int, end: Int): List[Int] = {
    if (end < start) Nil
    else start :: rangeMethod(start + 1, end)
  }

  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    def randomSelectR(n: Int, ls: List[A], r: util.Random): List[A] =
      if (n <= 0) Nil
      else {
        val (rest, e) = removeAt(r.nextInt(ls.length), ls)
        e :: randomSelectR(n - 1, rest, r)
      }
    randomSelectR(n, ls, new util.Random)  
  }

  def lotto[A](number: Int, range: Int): List[Int] = {

    val res = rangeMethod(number, range)
    randomSelect(number, res)
  }

  def randomPermute[A](ls: List[A]): List[A] = {
    randomSelect(ls.length, ls)
  }

  def combinations[A](number: Int, xs: List[A]): List[List[A]] = {
      def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] = 
        ls match {
           case Nil => Nil
           case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)  
        }  
      if (number == 0) List(Nil)
      else flatMapSublists(xs) { sl =>
        combinations(number - 1, sl.tail) map {sl.head :: _}
      }   
  }


  
}
