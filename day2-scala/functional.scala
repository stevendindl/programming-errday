
// val list = List(1, 2, 3, 4, 5)
// val sum = list.fold(0)((x, y) => x + y)
// assert(sum == 15)

// val map: (a -> b) -> [a] -> [b]  -- wrong
// def map[A,B]: (f: A => B) => (l: List[A]) => (List[B])=> -- wrong
//         l.fold(List.empty[B])((x,acc) => f(x) :: acc)

// map 
def map[A,B] (f: A => B) (l: List[A]): (List[B]) = 
        l.foldRight(List.empty[B])((x,acc) => f(x) :: acc)

// Helper function (using with map)
val multiplyBy2 = (x: Int) => x * 2

// reverse list
def rev[A] (l: List[A]): (List[A]) = 
        l.foldLeft(List.empty[A])((acc,x) => x :: acc)

@main
def Main(): Unit =

    // Default list
    val list = List(1, 2, 3, 4, 5)

    // Map x2
    val list2: List[Int] = map(multiplyBy2)(list)

    // Reverse list
    val listRev: List[Any] = rev(list)

    println("Default List:    " + list)
    println("2x mapped List:  " + list2)
    println("Reversed List:   " + listRev)
    println("Reversed 2xList: " + rev(list2))