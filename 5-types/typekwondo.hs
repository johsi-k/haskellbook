-- 1.
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

-- implement h
h :: Int -> Char
h x = g (f x)


-- 2.
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

-- implement e
e :: A -> C
e x = w (q x)


-- 3.
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

-- implement xform
xform :: (X, Y) -> (Z, Z)
xform (x, y) = ((xz x), (yz y))

-- 4.
munge :: (x -> y)
      -> (y -> (w, z))
      -> X
      -> w

-- implement munge
munge xToY yToWZ x = fst (yToWZ (xToY x))
