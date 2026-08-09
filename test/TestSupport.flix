def inheritedFleshDefinitions(): String =
    "data a option { none, a some } " +
    "bone f functor { (a f) map (a → b ! e): b f ! e } " +
    "bone f applicative need f functor { a pure: a f, (a f) apply ((a → b ! e) f): b f ! e } " +
    "bone f monad need f applicative { ((a f) f) flat: a f, (a f) bind (a → b f ! e): b f ! e } " +
    "flesh option monad { " +
    "value map f = match value { none ↦ none, a some ↦ (a f) some }, " +
    "a pure = a some, " +
    "value apply maybe-function = match value, maybe-function { a some, f some ↦ (a f) some, _, _ ↦ none }, " +
    "value flat = match value { none ↦ none, inner some ↦ inner }, " +
    "value bind f = (value map f) flat " +
    "} "
