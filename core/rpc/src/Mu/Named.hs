{-# language DataKinds              #-}
{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language GADTs                  #-}
{-# language PolyKinds              #-}
{-# language RankNTypes             #-}
{-# language TypeOperators          #-}
module Mu.Named where

import           GHC.TypeLits

-- | A value tagged with a type-level name.
data Named n h where
  Named :: forall n h. h -> Named n h

infixr 4 :|:
-- | Heterogeneous list in which each element
--   is tagged with a type-level name.
data NamedList (hs :: [(Symbol, *)]) where
  N0    :: NamedList '[]
  (:|:) :: Named n h -> NamedList hs
        -> NamedList ('(n, h) ': hs)

-- | Used to turn tuples into 'NamedList's.
class ToNamedList p nl | p -> nl where
  toNamedList :: p -> NamedList nl

instance ToNamedList (NamedList nl) nl where
  toNamedList = id
instance ToNamedList () '[] where
  toNamedList _ = N0
instance ToNamedList (Named n h) '[ '(n, h) ] where
  toNamedList n = n :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2)
                     '[ '(n1, h1), '(n2, h2) ] where
  toNamedList (n1, n2) = n1 :|: n2 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3) ] where
  toNamedList (n1, n2, n3) = n1 :|: n2 :|: n3 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3, Named n4 h4)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3), '(n4, h4) ] where
  toNamedList (n1, n2, n3, n4) = n1 :|: n2 :|: n3 :|: n4 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3, Named n4 h4, Named n5 h5)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3), '(n4, h4), '(n5, h5) ] where
  toNamedList (n1, n2, n3, n4, n5) = n1 :|: n2 :|: n3 :|: n4 :|: n5 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3, Named n4 h4, Named n5 h5, Named n6 h6)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3), '(n4, h4), '(n5, h5), '(n6, h6) ] where
  toNamedList (n1, n2, n3, n4, n5, n6) = n1 :|: n2 :|: n3 :|: n4 :|: n5 :|: n6 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3, Named n4 h4, Named n5 h5, Named n6 h6, Named n7 h7)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3), '(n4, h4), '(n5, h5), '(n6, h6), '(n7, h7) ] where
  toNamedList (n1, n2, n3, n4, n5, n6, n7) = n1 :|: n2 :|: n3 :|: n4 :|: n5 :|: n6 :|: n7 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3, Named n4 h4, Named n5 h5, Named n6 h6, Named n7 h7, Named n8 h8)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3), '(n4, h4), '(n5, h5), '(n6, h6), '(n7, h7), '(n8, h8) ] where
  toNamedList (n1, n2, n3, n4, n5, n6, n7, n8) = n1 :|: n2 :|: n3 :|: n4 :|: n5 :|: n6 :|: n7 :|: n8 :|: N0
instance ToNamedList (Named n1 h1, Named n2 h2, Named n3 h3, Named n4 h4, Named n5 h5, Named n6 h6, Named n7 h7, Named n8 h8, Named n9 h9)
                     '[ '(n1, h1), '(n2, h2), '(n3, h3), '(n4, h4), '(n5, h5), '(n6, h6), '(n7, h7), '(n8, h8), '(n9, h9) ] where
  toNamedList (n1, n2, n3, n4, n5, n6, n7, n8, n9) = n1 :|: n2 :|: n3 :|: n4 :|: n5 :|: n6 :|: n7 :|: n8 :|: n9 :|: N0
