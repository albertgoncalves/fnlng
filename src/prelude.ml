let (|.) (f : 'b -> 'c) (g : 'a -> 'b) : 'a -> 'c =
  fun (x : 'a) -> f (g x)
