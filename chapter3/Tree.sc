sealed trait MyTree[+A]
case class MyLeaf[A](value: A) extends MyTree[A]
case class MyBranch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {
  def mySize[A](tree: MyTree[A]): Int = {
    tree match  {
      case MyLeaf(_) => 1
      case MyBranch(l, r) => mySize(l) + mySize(r) +1
    }
  }

  def myMax(tree: MyTree[Int]): Int = {
    tree match {
      case MyBranch(l, r) => {
        myMax(l) max myMax(r)
      }
      case MyLeaf(v) => v
    }
  }


  def main(args: Array[String]): Unit = {
    val testLeaf = MyLeaf(4)
    val testLeaf2 = MyLeaf(3)
    val testTree = MyBranch(testLeaf, testLeaf2)
    val size  = myMax(testTree)
    println(s"$size")
  }
}
