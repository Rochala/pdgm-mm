object Lista2 {
    def find(list:List[String], patterns :List[String]) : List[String] = {
        (patterns)
        list.filter{word => word.contains(patterns.head)}
    }
}