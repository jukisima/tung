enum Ty {
    case Meta(Int32)
    case Var(String)
    case Con(String)
    case App(String, List[Ty])
    case Record(List[(String, Ty)])
    case Fun(List[Ty], List[String], Ty)
}

enum Inferred {
    case Value(Ty, List[String])
}

enum Scheme {
    case Forall(List[String], Ty)
}

enum EnvLookup {
    case Found(Scheme)
    case Missing
    case Ambiguous(String)
}

enum EnvChoices {
    case Choices(List[Scheme])
    case Missing
    case Ambiguous(String)
}

enum TcResult[a] {
    case Ok(a, TcState)
    case Err(String)
}

enum TcState {
    case State(Int32, List[(Int32, Ty)], List[(String, String)])
}

enum TypeInfo {
    case Alias(String, List[String])
    case Data(String, List[String], List[(String, List[TypeExpr])])
}

enum BoneInfo {
    case Info(List[String], List[BoneMember])
}

enum TcContext {
    case Context(List[(String, Scheme, Int32, String)], List[(String, TypeInfo)], List[(String, BoneInfo)])
}
