module TopologyExtras.Topology where

import UnambCustom.Unamb

data Σ = T deriving (Read,Show)
type Sierpinski = Σ

bottom :: Σ
bottom = error "Topology.bottom"

infixr 2 \/, /\
(\/) :: Σ -> Σ -> Σ
(\/) = unamb

(/\) :: Σ -> Σ -> Σ
T /\ x = x

infix 4 ===, =/=
class Discrete a  where (===) :: a -> a -> Σ
class Hausdorff a where (=/=) :: a -> a -> Σ
class Overt a     where forsome :: (a -> Σ) -> Σ
class Compact a   where forevery :: (a -> Σ) -> Σ


-- Instances

pred0 True = T
pred0 False = bottom
pred2 f x y = pred0 (f x y)

instance Overt Σ where forsome p = p T
instance Compact Σ where forevery p = p bottom

instance Discrete ()  where () === () = T
instance Hausdorff () where () =/= () = bottom
instance Overt ()     where forsome p = p ()
instance Compact ()   where forevery p = p ()

instance (Discrete a, Discrete b) => Discrete (a,b) where
    (x,y) === (x',y') = x === x' /\ y === y'
instance (Hausdorff a, Hausdorff b) => Hausdorff (a,b) where
    (x,y) =/= (x',y') = x =/= x' \/ y =/= y'
instance (Overt a, Overt b) => Overt (a,b) where
    forsome p = forsome (\x -> forsome (\y -> p (x,y)))
instance (Compact a, Compact b) => Compact (a,b) where
    forevery p = forevery (\x -> forevery (\y -> p (x,y)))

instance (Discrete a, Discrete b) => Discrete (Either a b) where
    Left x  === Left y  = x === y
    Right x === Right y = x === y
    _       === _       = bottom
instance (Hausdorff a, Hausdorff b) => Hausdorff (Either a b) where
    Left x  =/= Left y  = x =/= y
    Right x =/= Right y = x =/= y
    _       =/= _       = T
instance (Overt a, Overt b) => Overt (Either a b) where
    forsome p = forsome (p . Left) \/ forsome (p . Right)
instance (Compact a, Compact b) => Compact (Either a b) where
    forevery p = forevery (p . Left) /\ forevery (p . Right)

instance Discrete Bool where (===) = pred2 (==)
instance Hausdorff Bool where (=/=) = pred2 (/=)
instance Overt Bool where forsome p = p True \/ p False
instance Compact Bool where forevery p = p True /\ p False

instance Discrete  Int     where (===) = pred2 (==)
instance Hausdorff Int     where (=/=) = pred2 (/=)
instance Overt     Int     where
    forsome p = foldr1 (\/) (map p z)
        where
        z = interleave [0..] [-1,-2..]  
            -- not just [minBound..maxBound] because we want to 
            -- check "intereresting" values first
instance Compact   Int     where
    forevery p = foldr1 (/\) (map p z)
        where
        z = interleave [0..] [-1,-2..]

instance Discrete  Integer where (===) = pred2 (==)
instance Hausdorff Integer where (=/=) = pred2 (/=)
instance Overt     Integer where
    forsome p = foldr1 (\/) (map p z)
        where
        z = interleave [0..] [-1,-2..]

instance Discrete Rational where (===) = pred2 (==)
instance Hausdorff Rational where (=/=) = pred2 (/=)
instance Overt Rational where
    forsome p = foldr1 (\/) (map p q)
        where
        qplus = iterate next 0
        next x = recip (fromInteger n + 1 - y) where (n,y) = properFraction x
        q = interleave qplus (map negate (tail qplus))

instance (Discrete a) => Discrete (Maybe a) where
    Nothing === Nothing = T
    Just x  === Just y  = x === y
    _       === _       = bottom
instance (Hausdorff a) => Hausdorff (Maybe a) where
    Nothing =/= Nothing = bottom
    Nothing =/= Just _  = T
    Just _  =/= Nothing = T
    Just x  =/= Just y  = x =/= y
instance (Overt a) => Overt (Maybe a) where
    forsome p = p Nothing \/ forsome (p . Just)
instance (Compact a) => Compact (Maybe a) where
    forevery p = p Nothing /\ forevery (p . Just)

instance (Hausdorff a) => Hausdorff [a] where
    []     =/= []     = bottom
    (x:xs) =/= []     = T
    []     =/= (y:ys) = T
    (x:xs) =/= (y:ys) = (x =/= y) \/ (xs =/= ys)

interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x:y:interleave xs ys
