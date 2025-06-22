

class Tricks {

    val ValidStances: List[String] = List("fakie", "nollie", "switch", "regular")

    def switchStances(stance: String, tricks: List[String]): List[String] =
        if (ValidStances.contains(stance)) {
            return tricks.map((trick) => stance + " " + trick)
        } 
        return tricks
        
    def stanceDrop(t: String): String = {
        if (t.length < 8)
            return t
        else if (t.startsWith("nollie") || t.startsWith("switch"))
            return t.drop(7)
        else if (t.startsWith("fakie"))
            return t.drop(6)
        else
            return t
    }

    val TricksToRegular = Map(
        "nollie" -> "ollie",
        "nollie body varial" -> "ollie body varial",
        "nollie sex change" -> "ollie sex change"   // this is equal to the prev trick, should find a sol for this
        "nollie north"      -> "ollie north"
        "nose manual"    -> "manual"
        "half cab"       -> "bs 180"
        "full cab"       -> "bs 360"
    )
     
    // def stanceDrop(stance: String) =

}


@main
def Main(): Unit =
    val tricksClass = new Tricks()

    val trickList: List[String] = List(
        "kickflip", "switch tre flip", "inward heel flip", "ollie", 
        "nose manual", "fakie bigspin", "nollie shuv"
    )

    println(tricksClass.switchStances("fakie", trickList))
