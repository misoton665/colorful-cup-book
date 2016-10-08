package com.ccb

object Chapter4 {
  def main(args: Array[String]): Unit = {

    // Ex4.1
    val som: Option[Int] = Some(100)

    val map_ = som.map(_ * 2)
    val flatMap_ = som.flatMap(Some(_)).flatMap[Int]((_) => None).flatMap(x => Some(x * 2))
    val getOrElse_ = som.getOrElse(200)
    val orElse_ = som.orElse(Some(200))
    val filter_ = som.filter(_ % 3 == 0)

    println("map: " + map_)
    println("flatMap: " + flatMap_)
    println("getOrElse: " + getOrElse_)
    println("orElse: " + orElse_)
    println("filter: " + filter_)

    // Ex4.2
    val valSeq1 = Seq(1.0, -1.0, 1.0, -1.0, 1.0, -1.0)
    val valSeq2 = Seq(2.0, -2.0, 2.0, -2.0, 2.0, -2.0, 2.0)
    val valSeq3 = Seq()

    println("variance1: " + variance(valSeq1))
    println("variance2: " + variance(valSeq2))
    println("variance3: " + variance(valSeq3))

    // Ex4.3
    val some100 = Some(100)
    val some200 = Some(200)

    println("map2: " + Option.map2(some100, some200)(_ + _))

    // Ex4.4
    val sequenceSample1 = List(Some(1), Some(2), Some(3), Some(4), Some(5))
    val sequenceSample2 = List(Some(1), Some(2), None,    Some(4), Some(5))

    println("sequenceSample1: " + Option.sequence(sequenceSample1))
    println("sequenceSample2: " + Option.sequence(sequenceSample2))

    // Ex4.5
    val traverseSample = List(1, 2, 3, 4, 5)

    val traverseF1 = (a: Int) => Some(a)

    val traverseF2 = (a: Int) => a match {
      case 3 => None
      case aa@_ => Some(aa)
    }

    println("traverseSample1: " + Option.traverse(traverseSample)(traverseF1))
    println("traverseSample2: " + Option.traverse(traverseSample)(traverseF2))

    // Ex4.6
    val right: Either[String, Int] = Right(100)
    val left: Either[String, Int] = Left("left1")

    println("right map: " + right.map(_ * 2))
    println("left map: " + left.map(_ * 2))
    println("right flatMap: " + right.flatMap(e => Right(e * 2)).flatMap[String, Int](e => Left("left: " + e)).flatMap(e => Right(e * 2)))
    println("left flatMap: " + left.flatMap(e => Right(e * 2)).flatMap[String, Int](e => Left("left: " + e)).flatMap(e => Right(e * 2)))
    println("right orElse: " + right.flatMap(_ => Left("r -> l")))
    println("left orElse: " + left.flatMap(_ => Left("l -> l")))
    println("right map2: " + right.map2(left)(_ + _))
    println("left map2: " + left.map2(right)(_ + _))

    // Ex4.7
    val rightList = List(Right(1), Right(2), Right(3), Right(4), Right(5))
    val someLeftList = List(Right(1), Right(2), Left("left 3"), Right(4), Right(5))
    val valList = List(1, 2, 3, 4, 5)
    val f1 = (a: Int) => Right(a * 2)
    val f2 = (a: Int) => Left("left: " + a)

    println("rightList sequence: " + Either.sequence(rightList))
    println("someLeftList sequence: " + Either.sequence(someLeftList))
    println("valList traverse f1: " + Either.traverse(valList)(f1))
    println("valList traverse f2: " + Either.traverse(valList)(f2))
  }

  // Ex4.1
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = {
      this match {
        case Some(a) => Some(f(a))
        case None => None
      }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      this match {
        case Some(a) => f(a)
        case None => None
      }
    }

    def getOrElse[B >: A](default: => B): B = {
      this match {
        case Some(a) => a
        case None => default
      }
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      this match {
        case it@Some(a) => it
        case None => ob
      }
    }

    def filter(f: A => Boolean): Option[A] = {
      this match {
        case Some(a) =>
          if (f(a)) {
            Some(a)
          } else {
            None
          }

        case None => None
      }
    }
  }

  object Option {

    // Ex4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      (a, b) match {
        case (Some(sa), Some(sb)) => Some(f(sa, sb))
        case _ => None
      }
    }

    // Ex4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      a.foldLeft[Option[List[A]]](Some(List[A]())) { (acc, x) =>
        (acc, x) match {
          case (Some(someAcc), Some(someX)) => Some(someAcc :+ someX)
          case _ => None
        }
      }
    }

    // Ex4.5
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
      a.foldLeft[Option[List[B]]](Some(List[B]())) { (acc, x) =>
        (acc, f(x)) match {
          case (Some(someAcc), Some(someX)) => Some(someAcc :+ someX)
          case _ => None
        }
      }
    }
  }

  case class Some[A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  // Ex4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    val m = xs.sum

    if (xs.isEmpty) {
      None
    } else {
      Some(xs.map(x => math.pow(x * m, 2)).sum / xs.size)
    }
  }

  // Ex4.6
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = {
      this match {
        case left@Left(_) => left
        case Right(r) => Right(f(r))
      }
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case left@Left(_) => left
        case Right(r) => f(r)
      }
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(_) => b
        case right@Right(_) => right
      }
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      (this, b) match {
        case (Right(r1), Right(r2)) => Right(f(r1, r2))
        case (_, left@Left(_)) => left
        case (left@Left(_), Right(_)) => left
      }
    }
  }

  // Ex4.7
  object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      es.foldLeft[Either[E, List[A]]](Right(List())) { (acc, x) =>
        (acc, x) match {
          case (Right(rAcc), Right(rX)) => Right(rAcc :+ rX)
          case (left@Left(_), _) => left
          case (Right(_), left@Left(_)) => left
        }
      }
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      as.foldLeft[Either[E, List[B]]](Right(List())) { (acc, x) =>
        (acc, f(x)) match {
          case (Right(rAcc), Right(rX)) => Right(rAcc :+ rX)
          case (left@Left(_), _) => left
          case (Right(_), left@Left(_)) => left
        }
      }
    }
  }

  case class Left[+E](left: E) extends Either[E, Nothing]
  case class Right[+A](right: A) extends Either[Nothing, A]
}
