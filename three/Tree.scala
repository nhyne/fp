sealed trait MyTree[+A]

case class MyLeaf[A](value: A) extends MyTree[A]

case class MyBranch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {
    def mySize[A](tree: MyTree[A]): Int = {
        tree match {
            case MyLeaf(x) => 1
            case MyBranch(l, r) => mySize(l) + mySize(r) + 1
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

    def depth[A](tree: MyTree[A]): Int = {
        tree match {
            case MyBranch(l, r) => {
                1 + depth(l) max depth(r)
            }
            case MyLeaf(_) => 1
        }
    }

    def map[A, B](tree: MyTree[A])(f: A => B): MyTree[B] = {
        tree match {
            case MyLeaf(x) => MyLeaf(f(x))
            case MyBranch(l, r) => MyBranch(map(l)(f), map(r)(f))
        }
    }

    def fold[A, B](tree: MyTree[A], f: A => B, g: (B, B) => B): B = {
        tree match {
            case MyLeaf(x) => f(x)
            case MyBranch(l, r) => g(fold(l, f, g), fold(r, f, g))
        }
    }


    def main(args: Array[String]): Unit = {
        val testLeaf = MyLeaf(4)
        val testLeaf2 = MyLeaf(3)
        val testTree = MyBranch(testLeaf, testLeaf2)
        val max = myMax(testTree)
        println(s"max: $max")

        val treeSize = mySize(testTree)
        println(s"size: $treeSize")

        val treeDepth = depth(testTree)
        println(s"depth: $treeDepth")

        val mappedTree = map(testTree)(_ + 1)
        println(s"mapped: $mappedTree")

        val foldedTree = fold(testTree, (a: Int) => a + 1, (a: Int, b: Int) => a * b)
        println(s"folded: $foldedTree")
    }
}

