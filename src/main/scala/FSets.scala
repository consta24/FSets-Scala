import scala.annotation.tailrec

trait FSets {

  def singleton(x: Int): Int => Boolean = tmp => tmp == x


  def member(set: Int => Boolean)(e: Int): Boolean = set(e)


  def fromBounds(start: Int, stop: Int): Int => Boolean = x => x >= start && x <= stop



  def intersection(set1: Int => Boolean, set2: Int => Boolean): Int => Boolean = x => set1(x) && set2(x)

  def union(set1: Int => Boolean, set2: Int => Boolean): Int => Boolean = x => set1(x) || set2(x)



  def sumSet(start: Int, stop: Int, set: Int => Boolean): Int = {
    def auxSum(crt: Int, acc: Int): Int = {
      if (crt > stop) acc
      else if(!member(set)(crt)) auxSum(crt+1, acc)
      else auxSum(crt+1,acc+crt)
    }
    auxSum(start,0)
  }


  def foldSet(
               start: Int,            // bounds (inclusive)
               stop: Int,
               op: (Int, Int) => Int, // folding operation
               initial: Int,          // initial value
               set: Int => Boolean    // the set to be folded
             ): Int = {
    def aux(crt: Int, acc: Int): Int ={
      if(crt>stop) acc
      else if(!member(set)(crt)){
        aux(crt+1, acc)
      }
      else aux(crt+1,op(crt,acc))
    }
    aux(start, initial)
  }



  def forall(
              start: Int, // start value (inclusive)
              stop: Int, // stop value (inclusive)
              condition: Int => Boolean, // condition to be checked
              set: Int => Boolean // set to be checked
            ): Boolean = {
    @tailrec
    def iter(start: Int): Boolean = {
      if (start > stop) true
      else if (set(start) && !condition(start)) false
      else iter(start + 1)
    }
    iter(-stop)
  }



  def exists(
              start: Int, // start value (inclusive)
              stop: Int, // stop value (inclusive)
              condition: Int => Boolean, // condition to be checked
              set: Int => Boolean // set
            ): Boolean = !forall(start, stop, x => !condition(x), set)

}


object FSets extends FSets
