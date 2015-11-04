module Lens

import Control.Category

%access public

-- Store comonad

data Store s a = MkStore (s -> a) s

class Functor w => Comonad (w : Type -> Type) where
  extract : w a -> a
  extend : (w a -> b) -> w a -> w b

instance Functor (Store s) where
  map f (MkStore g a) = MkStore (Prelude.Basics.(.) f g) a

instance Comonad (Store s) where
  extract (MkStore f a) = f a
  extend f (MkStore g a) = MkStore (\b => f (MkStore g b)) a

pos : Store s a -> s
pos (MkStore _ s) = s

peek : s -> Store s a -> a
peek s (MkStore f _) = f s

peeks : (s -> s) -> Store s a -> a
peeks f (MkStore g s) = g (f s)

-- Lenses

data Lens a b = MkLens (a -> Store b a)

instance Category Lens where
  id = MkLens (MkStore id)
  (.) (MkLens f) (MkLens g) = MkLens (\a => case g a of
    MkStore ba b => case f b of
      MkStore cb c => MkStore (Prelude.Basics.(.) ba cb) c)

lens : (a -> b) -> (b -> a -> a) -> Lens a b
lens f g = MkLens (\a => MkStore (\b => g b a) (f a))

iso : (a -> b) -> (b -> a) -> Lens a b
iso f g = MkLens (\a => MkStore g (f a))

getL : Lens a b -> a -> b
getL (MkLens f) a = pos (f a)

setL : Lens a b -> b -> a -> a
setL (MkLens f) b = peek b . f

modL : Lens a b -> (b -> b) -> a -> a
modL (MkLens f) g = peeks g . f

mergeL : Lens a c -> Lens b c -> Lens (Either a b) c
mergeL (MkLens f) (MkLens g) = MkLens $ either (\a => map Left $ f a)
                                               (\b => map Right $ g b)

infixr 0 ^$
(^$) : Lens a b -> a -> b
(^$) = getL

infixr 4 ^=
(^=) : Lens a b -> b -> a -> a
(^=) = setL

infixr 4 ^%=
(^%=) : Lens a b -> (b -> b) -> a -> a
(^%=) = modL

fstLens : Lens (a,b) a
fstLens = MkLens $ \(a,b) => MkStore (\ a' => (a', b)) a

sndLens : Lens (a,b) b
sndLens = MkLens $ \(a,b) => MkStore (\ b' => (a, b')) b

-- Partial lenses

data PLens a b = MkPLens (a -> Maybe (Store b a))

instance Category PLens where
  id = MkPLens (Just . MkStore id)
  (.) (MkPLens f) (MkPLens g) = MkPLens (\a => do
    MkStore wba b <- g a
    MkStore wcb c <- f b
    return (MkStore (Prelude.Basics.(.) wba wcb) c))

plens : (a -> Either a (Store b a)) -> PLens a b
plens f = MkPLens $ either (const Nothing) Just . f

getPL : PLens a b -> a -> Maybe b
getPL (MkPLens f) a = map pos (f a)

justPL : PLens (Maybe a) a
justPL = MkPLens (\ma => do
  a <- ma
  return (MkStore Just a))
