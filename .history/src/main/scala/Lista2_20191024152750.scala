object Lista2 {
    def find(list:List[String], patterns :List[String]) : List[String] = {
        (patterns) match {
            case Nil find
        }
        list.filter{word => word.contains(patterns.head)}
    }
}