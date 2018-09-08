identity = \x -> x
apply = \f -> \x -> f x
true = \a -> \b -> a
false = \a -> \b -> b
val = identity true
