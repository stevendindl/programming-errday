// scala run hello.scala

object Printer {

    def printHello(x: Int) =
        for (i <- 0 to x) {
            println("Hello")
        }

    def print(string: String, n: Int) =
        for(i <- 0 to n)
            println(string)

    def drop(string: String) : String =
        var dropped: String = ""
        if (string.length > 0) {
            for(i <- 1 to (string.length - 1)) {
                dropped = dropped + string(i)
            }
        }
        return dropped

     def drop(string: String, num: Int) : String =
        var dropped: String = ""
        if (num < 0) {
            return string
        }
        if (string.length > num) {
            for(i <- num to (string.length - 1)) {
                dropped = dropped + string(i)
            }
        }
        return dropped
  

}
    

@main
def Hello(): Unit =
    Printer.printHello(5)
    println()

    Printer.print("Cheese", 5)
    println()

    println(Printer.drop("cheese"))
    println(Printer.drop("cheese", 3))
    println(Printer.drop("cheese", 1000000))
    println(Printer.drop("cheese", -1))
    println()

    var ImmutatableVar: String = "chunky cheese"
    for (i <- 0 to (ImmutatableVar.length - 1)) {
        println(Printer.drop(ImmutatableVar, i))
    }
    println()

    for (i <- (ImmutatableVar.length - 1) to 0 by -1) {
        println(Printer.drop(ImmutatableVar, i))
    }

