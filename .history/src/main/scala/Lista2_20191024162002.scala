object Lista2 {
    def find(list:List[String], patterns :List[String]) : List[String] = {
        (patterns,list) match {
            case (Nil,_) => Nil
            case (_,Nil) => Nil
            case (ph::pt, _) => list.filter{word => word.contains(ph)}++find(list,pt)
        }
    }

     def find2(list:List[String], patterns :List[String]) : List[String] = {
        def matchPatterns(list:List[String], patternsRep : List[String]) : List[String]= {
            (patterns) match {
                case Nil => Nil
                case (h::t) => if (w.contains(h)) element else matchPatterns(element, t)
            }
        }
        (list) match {
            case (Nil) => Nil
            case (h::t) => matchPatterns(h, patterns)::find2()
        }
    }

    def findRec(list:List[String], patterns :List[String]) : List[String] = {
        def helper(list:List[String], patterns :List[String], acc :List[String]) : List[String] = {
            (patterns,list) match {
            case (Nil,_) => Nil
            case (_,Nil) => 
            case (ph::pt, h::t) => list.filter{word => word.contains(ph)}++find(list,pt)
        }
        }

    }

}