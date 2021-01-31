package chapter5

import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  //def take(n: Int): Stream[A] = (n, this) match {
  //  case (0, _) => Empty
  //  case (_, Empty) => Empty
  //  case (_, Cons(h,t)) => cons(h(), t().take(n-1))
  //}
  def take(n: Int): Stream[A] = 
    unfold((n,this)) {
      case (0, _) => None
      case (_, Empty) => None
      case (n, Cons(h,t)) => Some((h(), (n-1, t())))
    }

  def drop(n: Int): Stream[A] = (n, this) match {
    case (0, _) => this
    case (_, Empty) => Empty
    case (n, Cons(h,t)) => t().drop(n-1)
  }
  
  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z 
    }
  }
  
  //def takeWhile(p: A => Boolean): Stream[A] = this match {
  //  case Empty => Empty
  //  case Cons(h,t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
  //}
  
  // takeWhile using foldRight
  //def takeWhile(p: A => Boolean): Stream[A] = 
  //  foldRight(empty[A])((h, t) => 
  //      if(p(h)) cons(h, t)
  //      else empty)
  
  // takeWhile using unfold
  def takeWhile(p: A => Boolean): Stream[A] = 
    unfold(this) {
      case Cons(h,t) => if(p(h())) Some((h(), t())) else None
      case _ => None
    }
    
  def zipWith[B](that: Stream[B]): Stream[(A,B)] = 
    unfold((this, that)) {
      case (Cons(h,t), Cons(h2,t2)) => Some(((h(),h2()),(t(),t2())))
      case (_, Empty) => None
      case (Empty, _) => None
    }

  def zipAll[B](that: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, that)) {
      case (Cons(h,t),Cons(h2,t2)) => 
                  Some(((Some(h()),Some(h2())), (t(),t2())))
      case (Cons(h,t), Empty) => 
                  Some(((Some(h()), None), (t(), Empty)))
      case (Empty, Cons(h2, t2)) => 
                  Some(((None, Some(h2())), (Empty, t2())))
      case _ => None
    }



  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  //def map[B](f: A => B): Stream[B] = 
  //  foldRight(empty[B])((h,t) => cons(f(h), t)) 
  def map[B](f: A => B): Stream[B] = 
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
          
  def filter(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((h,t) => if(p(h)) cons(h,t) else t)
  
  def append[B >: A](that: => Stream[B]): Stream[B] = 
    foldRight(that)(cons(_, _))
  
  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((h,t) => cons(f(h).toList(0),t))
  
  def find(p: A => Boolean): Option[A] = 
    filter(p).headOption
  
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /*
  def constant[A](a: A): Stream[A] =
    cons[A](a, Stream.constant(a))
  */
  def constant[A](a: A): Stream[A] =
    unfold(a)(z => Some((z,z)))
  
  /*
  def from(n: Int): Stream[Int] =
    cons[Int](n, Stream.from(n+1))
  */
  def from(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x+1)))

  /*
  def fibs(): Stream[Int] = {
    def f(nminus1: Int, nminus2:Int): Stream[Int] = 
      (nminus1, nminus2) match {
        case (0, -1) => cons(0, f(0, 0))
        case (0, 0) => cons(1, f(1, 0))
        case (n1, n2) => cons(n1+n2, f(n1+n2, n1))
      }
    f(0, -1)
  }
  */
  def fibs(): Stream[Int] = 
    unfold((-1, 1)){case (n2,n1) => Some(n2+n1, (n1,n2+n1))} 


  def unfold[A, S](z: S)(f: S => Option[(A,S)]): Stream[A] = 
    f(z) match {
      case None => empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  
}

