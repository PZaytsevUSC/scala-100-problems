import java.util.NoSuchElementException
// P1 - Find the last element of a list

def last(l: List[Int]): Int = {
  l.last
}

def lastRecursive(l: List[Int]): Int = l match {
  case head :: Nil => head
  case _ :: tail => lastRecursive(tail)
  case _ => throw new NoSuchElementException
}

lastRecursive(List(1, 2, 3, 4, 5))

// P2 - Find the last but one element of a list

def lastButOne(l: List[Int]): Int = l match {
  case head :: _ :: Nil => head
  case _ :: tail => lastButOne(tail)
  case _ => throw new NoSuchElementException
}

lastButOne(List(1, 2, 3, 4, 5))

// P3 - Find the Kth element of a list

def kth(k: Int, l: List[Int]): Int = l match {
  case head :: _ if k == 0 => head
  case _ :: tail if k > 0 => kth(k - 1, tail)
  case _ => throw new NoSuchElementException
}

kth(3, List(1, 2, 3, 4, 5))

// P4 - Find the number of elements of a list

def numElements(l: List[Int]): Int = {
  l.fold(0)((x, _) => x + 1)
}

// 0, 0 + 1 => 1 => 1 + 1 => 2 ...

numElements(List(1, 2, 3, 4))

// P5 - Reverse a list

def reverse(l: List[Int], l2: List[Int]): List[Int] = l match {
  case head :: Nil => head :: l2
  case head :: tail => reverse(tail, head :: l2)
  case _ => Nil
}

reverse(List(1, 2, 3), List())

// P6 - Find out whether a list is a palindrome

def palindrome(l: List[Int]): Boolean = {
  val reversed = reverse(l, List())
  def helper(l1: List[Int], l2: List[Int]): Boolean = (l1, l2) match {
    case (head :: tail, head2 ::tail2) if head == head2 => helper(tail, tail2)
    case (head :: _, head2 :: _) if head != head2 => false
    case (Nil, Nil) => return true
    case (_, _) => false
  }
  helper(l, reversed)
}

palindrome(List(1, 2, 3, 2, 1))
palindrome(List(1, 2, 3, 3, 2, 1))
palindrome(List(1, 2, 3, 4, 2, 1))

// P7 - Flatten a nested list structure

def flatten(l : List[Any]): List[Any] = {
  l.flatMap{
    case head :: tail => flatten(head :: tail)
    case e => List(e) // identity
  }
}

// List(List(List(1, 2, 3))) => List(List(1, 2, 3)) => List(List(1), List(2), List(3))
// then call combine with identity map, which is a foldRight with append
// => List(1, 2, 3)

// P8 - Eliminate consecutive duplicates of list elements

def compress(l: List[Int]): List[Int] = {
  l.foldRight(List.empty[Int])(
    (num, list) =>
      if (list.isEmpty || num != list.head){
        num :: list
      }
    else{
        list
      }
  )
}

compress(List(1, 1, 1, 2, 3, 3, 3, 4, 5, 6))

// Yummly interview problem: double nth element in depth first inorder traversal

sealed trait Tree
case class Node(left: Tree, right: Tree, value: Int) extends Tree
case class Leaf(value: Int) extends Tree

def size(tree: Tree): Int = tree match {
  case Node(left, right, _) => 1 + size(left) + size(right)
  case Leaf(_) => 1
}

def doubleNth(n: Int, tree: Tree): Tree = tree match {
  case Node(left, right, value) if n > 0 => Node(doubleNth(n - 1, left), doubleNth(n - size(left) - 1, right), value)
  case Node(left, right, value) if n == 0 => Node(left, right, value * value)
  case Node(left, right, value) if n < 0 => Node(left, right, value)
  case Leaf(value) if n > 0 => Leaf(value)
  case Leaf(value) if n == 0 => Leaf(value * value)
  case Leaf(value) if n < 0 => Leaf(value)
}

val n2 = Leaf(2)
val n3 = Leaf(3)
val n5 = Leaf(5)
val n6 = Leaf(6)
val n1 = Node(n2, n3, 1)
val n4 = Node(n5, n6, 4)
val n0 = Node(n1, n4, 0)

val res = doubleNth(4, n0)

// P9 - Pack consecutive duplicates of list elements into sublists

def pack(a: List[Int]): List[List[Int]] = {

  val l: List[List[Int]] = List(Nil)
  a.foldRight(l)(
    (num, lofl) =>

      if(lofl.head.isEmpty){
        val a: List[List[Int]] = (num :: lofl.head) :: lofl
        List(a.head)
      }
      else if(lofl.head.head == num || lofl.head.isEmpty){

        (num :: lofl.head) :: lofl.tail
      }
      else{
        (num :: List.empty[Int]) :: lofl
        }
  )
}

val packs = pack(List(1, 2, 3, 3, 3, 3, 4, 4, 5, 5, 6, 7, 7))

// P10 - Run-length encoding of a list
def encoding(a: List[Int]): List[(Int, Int)] = {
  val packs = pack(a)
  packs.map(x => (x.length, x.head))
}

val en = encoding(List(1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 5, 6, 6, 6))

// P11 - Modified run-length encoding

def modifiedEncoding(a: List[Int]): List[Any] = {
  val packs = pack(a)
  packs.map(x => if(x.length > 1) (x.length, x.head) else x.head)
}

modifiedEncoding(List(1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 5, 6, 6, 6))

// Function is sorted
def isSorted[A](a: Array[A], p: (A, A) => Boolean): Boolean = {
  def go(current: Int, next: Int): Boolean = {
    if(next <= a.length - 1 && p(a(current), a(next))){
      go(current + 1, next + 1)
    }
    else if(next == a.length){
      true
    }
    else{
      false
    }
  }

  if(a.isEmpty || a.length == 1){
    return true
  }
  go(0, 1)
}

val a = Array(1, 2, 3, 4, 5, 6)
val b = Array(1, 2, 4, 5, 3, 6)

isSorted(a, (a: Int, b: Int) => a < b)
isSorted(b, (a: Int, b: Int) => a < b)

def isSortedWithBounds[A](a: Array[A])(implicit ord: Ordering[A]): Boolean = {
  def go(current: Int, next: Int): Boolean = {
    if(next <= a.length - 1 && ord.lt(a(current), a(next))){
      go(current + 1, next + 1)
    }
    else if(next == a.length){
      true
    }
    else{
      false
    }
  }

  if(a.isEmpty || a.length == 1){
    return true
  }
  go(0, 1)
}

import Ordering.IntOrdering

isSortedWithBounds(a)

// currying

def partial[A, B, C](a: A, f: (A, B) => C): B => C = {
  (b: B) => f(a, b)
}

def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
  (a: A) => partial(a, f)
}


sealed trait GenTree[A]
case class GenLeaf[A](value: A) extends GenTree[A]
case class GenNode[A](value: A, left: GenTree[A], right: GenTree[A]) extends GenTree[A]

def treeFold[A, B](tree: GenTree[A], b: B)(op: (B, A) => B)(comb: (A, B, B) => B): B = tree match{
  case GenNode(value, left, right) => comb(value, treeFold(left, b)(op)(comb), treeFold(right, b)(op)(comb))
  case GenLeaf(value) => op(b, value)
}

val node4 = GenLeaf(4)
val node5 = GenLeaf(5)
val node6 = GenLeaf(6)
val node7 = GenLeaf(7)

val node2 = GenNode(2, node4, node5)
val node3 = GenNode(3, node6, node7)
val node1 = GenNode(1, node2, node3)

def size[A](tree: GenTree[A]): Int = {
  treeFold(tree, 0)((a: Int, b: A) => a + 1)((c: A, a: Int, b: Int) => 1 + a + b)
}

size(node1)

def maximum[A](tree: GenTree[A])(implicit ord: Ordering[A], nullable: A): A = {
  treeFold(tree, nullable)((a: A, b: A) => ord.max(a, b))((c: A, a: A, b: A) => ord.max(c, ord.max(a, b)))
}

implicit val nullable: Int = 0

maximum(node1)

def pathLength[A](tree: GenTree[A])(implicit ord: Ordering[Int]): Int = {
  treeFold(tree, 0)((a: Int, b: A) => 0)((c: A, a: Int, b: Int) => 1 + ord.max(a, b))
}

pathLength(node1)

