-- Ex 1.2-i
card12i = 2 ^ (sizeBool + sizeBool * sizeMBool)
  where sizeBool  = 2
        sizeMBool = sizeBool + 1

-- Ex1.4-i
f14i :: (b -> c -> a) -> (b, c) -> a
f14i = uncurry

g14i :: ((b, c) -> a) -> (b -> c -> a)
g14i = curry

-- Ex1.4-ii
f14ii :: (b -> a, c -> a) -> Either b c -> a
f14ii (ba, _) (Left b) = ba b
f14ii (_, ca) (Right c) = ca c

g14ii :: (Either b c -> a) -> (b -> a, c -> a)
g14ii h = (\b -> (h . Left) b, \c -> (h . Right) c)

-- Ex1.4-iii
f14iii :: (c -> (a, b)) -> (c -> a, c -> b)
f14iii h = (fst . h, snd . h)

g14iii :: (c -> a, c -> b) -> (c -> (a, b))
g14iii (ca, cb) = (\c -> (ca c, cb c))
