foo5 a b c = map (\(f, g) -> f c (g a b) == g (f c a) (f c b))
