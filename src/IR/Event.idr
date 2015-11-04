module IR.Event

import IR.Lens

KeyCode : Type
KeyCode = Int

record Key where
  constructor MkKey
  keyCode : KeyCode
  keyHasAlt, keyHasCmd, keyHasCtrl, keyHasShift : Bool

instance Eq Key where
  (==) (MkKey a b c d e) (MkKey a' b' c' d' e') = a == a' && b == b' && c == c' && d == d' && e == e'

-- Should be the Monoid instance:
infixl 3 <!>
(<!>) : Ordering -> Ordering -> Ordering
(<!>) EQ r = r
(<!>) l  r = l

-- Should be the Ord instance:
compareBool : Bool -> Bool -> Ordering
compareBool False False = EQ
compareBool False True = LT
compareBool True False = GT
compareBool True True = EQ

instance Ord Key where
  compare (MkKey a b c d e) (MkKey a' b' c' d' e') = compare a a'
                                                 <!> compareBool b b'
                                                 <!> compareBool c c'
                                                 <!> compareBool d d'
                                                 <!> compareBool e e'

keyCode' : Lens Key KeyCode
keyCode' = lens (\(MkKey x _ _ _ _) => x) (\x, (MkKey _ a b c d) => MkKey x a b c d)

keyHasAlt' : Lens Key Bool
keyHasAlt' = lens (\(MkKey _ x _ _ _) => x) (\x, (MkKey a _ b c d) => MkKey a x b c d)

keyHasCmd' : Lens Key Bool
keyHasCmd' = lens (\(MkKey _ _ x _ _) => x) (\x, (MkKey a b _ c d) => MkKey a b x c d)

keyHasCtrl' : Lens Key Bool
keyHasCtrl' = lens (\(MkKey _ _ _ x _) => x) (\x, (MkKey a b c _ d) => MkKey a b c x d)

keyHasShift' : Lens Key Bool
keyHasShift' = lens (\(MkKey _ _ _ _ x) => x) (\x, (MkKey a b c d _) => MkKey a b c d x)

data Event = KeyEvent Key
           | RefreshEvent
           | IgnoredEvent

eventFromPtr : Ptr -> IO Event
eventFromPtr p = do
  t <- foreign FFI_C "irEventType" (Ptr -> IO Int) p
  c <- foreign FFI_C "irEventKeyCode" (Ptr -> IO Int) p
  alt <- map (/= 0) (foreign FFI_C "irEventKeyAlternate" (Ptr -> IO Int) p)
  cmd <- map (/= 0) (foreign FFI_C "irEventKeyCommand" (Ptr -> IO Int) p)
  ctrl <- map (/= 0) (foreign FFI_C "irEventKeyControl" (Ptr -> IO Int) p)
  shift <- map (/= 0) (foreign FFI_C "irEventKeyShift" (Ptr -> IO Int) p)
  foreign FFI_C "irEventFree" (Ptr -> IO Unit) p
  return (case t of
    0 => KeyEvent (MkKey c alt cmd ctrl shift)
    1 => RefreshEvent
    _ => IgnoredEvent)
