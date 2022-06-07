import FSets._

class FSetTest extends munit.FunSuite {

  val emptySet : Int => Boolean = x => false


  test("Empty set ") {
    assert(!(member(x => false)(10)))
  }

  test("Singleton member test 1 ") {
    val s = singleton(10)
    assert(member(s)(10))
  }

  test("Singleton member test 2") {
    val s = singleton(10)
    assert(!member(s)(5))
  }

  test("fromBounds") {
    val three = fromBounds(1,3)
    assert(member(three)(1) && member(three)(2) && member(three)(3))
  }

  test("Intersection with empty set") {
    val s = singleton(10)
    assert(!member(intersection(s,emptySet))(10))
  }

  test("Intersection with singleton set"){
    val s = singleton(10)
    assert(member(intersection(s,singleton(10)))(10))
  }
  // (12p) 4. union - 3 tests
  test("Union with empty set") {
    val s = singleton(10)
    assert(member(union(s, emptySet))(10))
  }

  test("Union with two singleton sets, lhs") {
    val s = singleton(10)
    assert(member(union(s, singleton(5)))(5))
  }

  test("Union with two singleton sets, rhs") {
    val s = singleton(10)
    assert(member(union(s,singleton(5)))(10))
  }
  // (10p) 5. sumSet - 2 tests
  test("sumSet with empty set") {
    assert(sumSet(5, 10, emptySet) == 0)
  }

  test("sumSet with bounds"){
    val set = fromBounds(1, 20)
    assert(sumSet(5,10,set) == 45)
  }
  // (15p) 6. foldSet - 3 tests
  test("foldSet with empty set") {
    assert(foldSet(5, 10, _ + _, 0, emptySet) == 0)
  }

  test("foldSet with addition") {
    val set = fromBounds(1, 20)
    assert(foldSet(5, 10, _ + _, 0, set) == 45)
  }

  test("foldSet with multiplication") {
    val set = fromBounds(1, 20)
    assert(foldSet(5,10,_ * _, 1, set) == 151200)

  }
  // (15p) 7. forall - 3 tests
  test("Forall with empty set"){
    assert(forall(0,100,x => false, emptySet))
  }

  test("Forall with all even numbers") {
    val evens: Int => Boolean = x => x % 2 == 0
    assert(forall(0, 100, x => x % 2 == 0, evens))
  }

  test("Forall with one odd number") {
    val evens: Int => Boolean = x => x % 2 == 0
    assert(!forall(0,100, x => x % 2 == 0, union(evens,singleton(99))))
  }
  // (15p) 8. exists - 3 tests
  test("Exists with empty set") {
    assert(!exists(0, 100, x => true, emptySet))
  }

  test("Exists odd number in set of all-even numbers") {
    val evens: Int => Boolean = x => x % 2 == 0
    assert(!exists(0, 100, x => x % 2 == 1, evens))
  }

  test("Exists - odd number in set with one odd number") {
    val evens: Int => Boolean = x => x % 2 == 0
    assert(exists(0,100, x => x % 2 == 1, union(evens,singleton(99))))
  }

}

