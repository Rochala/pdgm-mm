object Lista2 {
    def find[A](list:List[A], patterns :List[A]) : List[A] = {
        (list,patterns) match {
            case (Nil,_) => Nil
            case (_,Nil) => find(list.tail)
            case (head::tail,phead::ptail) => 

        }
    }
}