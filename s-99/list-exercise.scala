

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

  
}
