def find(list:List[String], patterns :List[String]) : List[String] = {
        (patterns,list) match {
            case (Nil,_) => Nil
            case (_,Nil) => Nil
            case (ph::pt, h::t) => list.filter{word => word.contains(ph)}::find(list,pt)
        }
            list.distinct;
        
      
    }

    print(find(List("dupa", "123dupa","jarek","dupeczka"), List("dupa")))