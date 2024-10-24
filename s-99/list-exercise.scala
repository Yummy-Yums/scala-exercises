

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

  println(compressRecursive(List(Symbol("a"), Symbol("a"), Symbol("a"), Symbol("a"), Symbol("b"), Symbol("c"), Symbol("c"), Symbol("a"), Symbol("a"), Symbol("d"), Symbol("e"), Symbol("e"), Symbol("e"), Symbol("e"))
))

  println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
  
  println(findKthElement(2, List(1, 1, 2, 3, 5, 8)))
  println(findKthElement(3, List("o", "p", "l", "a")))
  println(isPalindrome(List(1, 2, 3, 2, 1)))
  println(isPalindrome(List(1, 2, 3)))
    
  println("Hello world!")
  
}
