object Lista2 {
    def find(list:List[String], patterns :List[String]) : List[String] = {
        (patterns,list) match {
            case (_,Nil) Nil
            case (Nil,h::t) list.filter{word => word.contains(patten)}
        }
        list.filter{word => word.contains(patterns.head)}
    }
}