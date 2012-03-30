!SLIDE
# <img src="typeclass/scala-logo.png"/> Type Classes 

<br/>
<br/>
@ppurang
<br/>
http://github.com/ppurang

<br/>
PLUG!

<a href="http://www.meetup.com/Scala-Berlin-Brandenburg/">http://www.meetup.com/Scala-Berlin-Brandenburg/</a>

!SLIDE

# What are type classes?

+ Type classes first appeared in Haskell to support ad-hoc polymorphism.
+ Ad-hoc polymorphism => A function is defined over many types, acting differently for each one. 
+ 1989: Philip Wadler & Stephen Blott, <nobr>
<a href="http://homepages.inf.ed.ac.uk/wadler/papers/class/class.ps">How to make ad-hoc polymorphism less ad hoc</a></nobr>
+ It is all compile time


!SLIDE

# Eq

    class Eq a where
      (==) :: a -> a -> Bool
      (/=) :: a -> a -> Bool

    instance Eq Integer where 
      x == y =  x `integerEq` y

    instance (Eq a) => Eq (Tree a) where 
      leaf a         == Leaf b          =  a == b
      (Branch l1 r1) == (Branch l2 r2)  =  (l1==l2) && (r1==r2)
      _              == _               =  False


!SLIDE

### Haskell Type Class is a Language Feature

### Scala Type Class is a Pattern or Recipe 


!SLIDE

# Ord in haskell

    class  (Eq a) => Ord a  where
      (<), (<=), (>=), (>)  :: a -> a -> Bool
      max, min              :: a -> a -> a

    quicksort :: Ord a => [a] -> [a]
    quicksort []     = []
    quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
        where
            lesser  = filter (< p) xs
            greater = filter (>= p) xs






!SLIDE


# A possible Ord in Scala
    type BooleanCompare[A] = A => A => Boolean; import Predef.{implicitly => ?}

    trait Ord[A] {
      def max : A => A => A ; def min : A => A => A
      def < : BooleanCompare[A]; def > : BooleanCompare[A]; def <= : BooleanCompare[A]; def >= : BooleanCompare[A]
    }

    implicit object IntOrd extends Ord[Int] {
      def max = a => b => if (a > b) a else b; def min = a => b => if (a > b) b else a
      def < = a => b => a < b; def > = a => b => a > b
      def <= = a => b => a <= b; def >= = a => b => a >= b
    }

    //implicits
    def quickSort1[A](l: List[A])(implicit x: Ord[A]): List[A] = l match {
      case p :: xs => quickSort1(xs filter (y => x.<(p)(y))) ::: p :: quickSort1(xs filter (y => x.>=(p)(y)))
      case _ => Nil
    }
    //context bounds
    def quickSort2[A: Ord](l: List[A]): List[A] = l match {
      case p :: xs => quickSort2(xs filter (y => ?[Ord[A]].<(p)(y))) ::: 
                      p :: 
                      quickSort2(xs filter (y => ?[Ord[A]].>=(p)(y)))
      case _ => Nil
    }



!SLIDE


+ Scala allows us to enhance a type by declaring implicit conversions to a newer wrapper type <nobr>(Pimp my class pattern)</nobr>
+ Scala allows us to define type classes and then make them available unintrusively using implicits (Think Defaults)
+ But that default can be overriden if needed by passing another implementation.

!SLIDE

# Tip of the Iceberg

    
    type Show[A] = A => String
    
    type Read[A] = String => A
    
    etc. etc.
    
For more on type classes and other functional goodness:

<br/>
<a href="https://github.com/scalaz/scalaz">https://github.com/scalaz/scalaz</a>
  

      
!SLIDE

# Thank You
<br>

## @ppurang

<br/>

## http://github.com/ppurang


<br>

### Further Reads
<a href="https://gist.github.com/2245812">https://gist.github.com/2245812</a>

<br>

### PLUG again!
<a href="http://www.meetup.com/Scala-Berlin-Brandenburg/">http://www.meetup.com/Scala-Berlin-Brandenburg/</a>

