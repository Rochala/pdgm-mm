object Lista2 {
    def find(list:List[String], patterns :List[String]) : List[String] = {
        (patterns,list) match {
            case (_,Nil) Nil
            case (Nil,h::t) find (list.)
        }
        list.filter{word => word.contains(patterns.head)}
    }
}