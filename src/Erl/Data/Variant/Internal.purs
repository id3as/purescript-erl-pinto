module Erl.Data.Variant.Internal
  ( VariantRep(..)
  , VariantCase
  , VariantFCase
  , class VariantTags
  , variantTags
  , class Contractable
  , contractWith
  , class VariantMatchCases
  , class VariantFMatchCases
  , lookup
  , lookupTag
  , lookupEq
  , lookupOrd
  , lookupLast
  , lookupFirst
  , lookupPred
  , lookupSucc
  , lookupCardinality
  , lookupFromEnum
  , lookupToEnum
  , BoundedDict
  , BoundedEnumDict
  , impossible
  , module Exports
  ) where

import Prelude
import Control.Alternative (class Alternative, empty)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Maybe as M
import Data.Symbol (class IsSymbol)
import Erl.Atom.Symbol (Atom, atom, toAtom)
import Erl.Atom as ErlAtom
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as R
import Prim.RowList as RL
import Erl.Record.Unsafe (unsafeGet, unsafeHas) as Exports
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

newtype VariantRep a
  = VariantRep
  { type ∷ ErlAtom.Atom
  , value ∷ a
  }

class VariantMatchCases :: RL.RowList Type -> Row Type -> Type -> Constraint
class VariantMatchCases rl vo b | rl → vo b

instance variantMatchCons ∷
  ( VariantMatchCases rl vo' b
  , R.Cons sym a vo' vo
  , TypeEquals k (a → b)
  ) ⇒
  VariantMatchCases (RL.Cons sym k rl) vo b

instance variantMatchNil ∷
  VariantMatchCases RL.Nil () b

class VariantFMatchCases :: RL.RowList Type -> Row (Type -> Type) -> Type -> Type -> Constraint
class VariantFMatchCases rl vo a b | rl → vo a b

instance variantFMatchCons ∷
  ( VariantFMatchCases rl vo' a b
  , R.Cons sym f vo' vo
  , TypeEquals k (f a → b)
  ) ⇒
  VariantFMatchCases (RL.Cons sym k rl) vo a b

instance variantFMatchNil ∷
  VariantFMatchCases RL.Nil () a b

foreign import data VariantCase ∷ Type

foreign import data VariantFCase ∷ Type → Type

class VariantTags :: forall k. RL.RowList k -> Constraint
class VariantTags rl where
  variantTags ∷ forall proxy. proxy rl → L.List ErlAtom.Atom

instance variantTagsNil ∷ VariantTags RL.Nil where
  variantTags _ = L.Nil

instance variantTagsCons ∷ (VariantTags rs, IsSymbol sym) ⇒ VariantTags (RL.Cons sym a rs) where
  variantTags _ = L.Cons (toAtom $ (atom ∷ Atom sym)) (variantTags (Proxy ∷ Proxy rs))

-- | A specialized lookup function which bails early. Foldable's `elem`
-- | is always worst-case.
lookupTag ∷ ErlAtom.Atom → L.List ErlAtom.Atom → Boolean
lookupTag tag = go
  where
  go = case _ of
    t L.: ts
      | t == tag → true
      | otherwise → go ts
    L.Nil → false

lookupEq ∷
  L.List ErlAtom.Atom →
  L.List (VariantCase → VariantCase → Boolean) →
  VariantRep VariantCase →
  VariantRep VariantCase →
  Boolean
lookupEq tags eqs (VariantRep v1) (VariantRep v2)
  | v1.type == v2.type = lookup (ErlAtom.atom "eq") v1.type tags eqs v1.value v2.value
  | otherwise = false

lookupOrd ∷
  L.List ErlAtom.Atom →
  L.List (VariantCase → VariantCase → Ordering) →
  VariantRep VariantCase →
  VariantRep VariantCase →
  Ordering
lookupOrd tags ords (VariantRep v1) (VariantRep v2) =
  case compare v1.type v2.type of
    EQ → lookup (ErlAtom.atom "compare") v1.type tags ords v1.value v2.value
    cp → cp

lookup ∷
  ∀ a.
  ErlAtom.Atom →
  ErlAtom.Atom →
  L.List ErlAtom.Atom →
  L.List a →
  a
lookup name tag = go
  where
  go = case _, _ of
    L.Cons t ts, L.Cons f fs
      | t == tag → f
      | otherwise → go ts fs
    _, _ → impossible name

lookupLast ∷
  ∀ a b.
  ErlAtom.Atom →
  (a → b) →
  L.List ErlAtom.Atom →
  L.List a →
  { type ∷ ErlAtom.Atom, value ∷ b }
lookupLast name f = go
  where
  go = case _, _ of
    L.Cons t L.Nil, L.Cons x L.Nil → { type: t, value: f x }
    L.Cons _ ts, L.Cons _ xs → go ts xs
    _, _ → impossible name

lookupFirst ∷
  ∀ a b.
  ErlAtom.Atom →
  (a → b) →
  L.List ErlAtom.Atom →
  L.List a →
  { type ∷ ErlAtom.Atom, value ∷ b }
lookupFirst name f = go
  where
  go = case _, _ of
    L.Cons t _, L.Cons x _ → { type: t, value: f x }
    _, _ → impossible name

lookupPred ∷
  ∀ a.
  VariantRep a →
  L.List ErlAtom.Atom →
  L.List (BoundedDict a) →
  L.List (BoundedEnumDict a) →
  Maybe (VariantRep a)
lookupPred (VariantRep rep) = go1
  where
  go1 = case _, _, _ of
    L.Cons t1 ts1, L.Cons b1 bs1, L.Cons d1 ds1
      | t1 == rep.type →
        case d1.pred rep.value of
          Nothing → Nothing
          Just z → Just $ VariantRep { type: rep.type, value: z }
      | otherwise → go2 t1 b1 d1 ts1 bs1 ds1
    _, _, _ → impossible (ErlAtom.atom "pred")

  go2 t1 b1 _ = case _, _, _ of
    L.Cons t2 ts2, L.Cons b2 bs2, L.Cons d2 ds2
      | t2 == rep.type →
        case d2.pred rep.value of
          Nothing → Just $ VariantRep { type: t1, value: b1.top }
          Just z → Just $ VariantRep { type: rep.type, value: z }
      | otherwise → go2 t2 b2 d2 ts2 bs2 ds2
    _, _, _ → impossible (ErlAtom.atom "pred")

lookupSucc ∷
  ∀ a.
  VariantRep a →
  L.List ErlAtom.Atom →
  L.List (BoundedDict a) →
  L.List (BoundedEnumDict a) →
  Maybe (VariantRep a)
lookupSucc (VariantRep rep) = go
  where
  go = case _, _, _ of
    L.Cons t1 ts1, L.Cons _ bs1, L.Cons d1 ds1
      | t1 == rep.type →
        case d1.succ rep.value of
          Just z → Just $ VariantRep { type: t1, value: z }
          Nothing → case ts1, bs1 of
            L.Cons t2 _, L.Cons b2 _ → Just $ VariantRep { type: t2, value: b2.bottom }
            _, _ → Nothing
      | otherwise → go ts1 bs1 ds1
    _, _, _ → impossible (ErlAtom.atom "succ")

lookupCardinality ∷
  ∀ a.
  L.List (BoundedEnumDict a) →
  Int
lookupCardinality = go 0
  where
  go acc = case _ of
    L.Cons d ds → go (acc + d.cardinality) ds
    L.Nil → acc

lookupFromEnum ∷
  ∀ a.
  VariantRep a →
  L.List ErlAtom.Atom →
  L.List (BoundedEnumDict a) →
  Int
lookupFromEnum (VariantRep rep) = go 0
  where
  go acc = case _, _ of
    L.Cons t ts, L.Cons d ds
      | t == rep.type → acc + d.fromEnum rep.value
      | otherwise → go (acc + d.cardinality) ts ds
    _, _ → impossible (ErlAtom.atom "fromEnum")

lookupToEnum ∷
  ∀ a.
  Int →
  L.List ErlAtom.Atom →
  L.List (BoundedEnumDict a) →
  Maybe (VariantRep a)
lookupToEnum = go
  where
  go ix = case _, _ of
    L.Cons t ts, L.Cons d ds
      | d.cardinality > ix →
        case d.toEnum ix of
          Just a → Just $ VariantRep { type: t, value: a }
          _ → Nothing
      | otherwise → go (ix - d.cardinality) ts ds
    _, _ → Nothing

class Contractable :: forall k. Row k -> Row k -> Constraint
class Contractable gt lt where
  contractWith ∷ ∀ proxy1 proxy2 f a. Alternative f ⇒ proxy1 gt → proxy2 lt → ErlAtom.Atom → a → f a

instance contractWithInstance ∷
  ( RL.RowToList lt ltl
  , R.Union lt a gt
  , VariantTags ltl
  ) ⇒
  Contractable gt lt where
  contractWith _ _ tag a
    | lookupTag tag (variantTags (Proxy ∷ Proxy ltl)) = pure a
    | otherwise = empty

type BoundedDict a
  = { top ∷ a
    , bottom ∷ a
    }

type BoundedEnumDict a
  = { pred ∷ a → M.Maybe a
    , succ ∷ a → M.Maybe a
    , fromEnum ∷ a → Int
    , toEnum ∷ Int → M.Maybe a
    , cardinality ∷ Int
    }

impossible ∷ ∀ a. ErlAtom.Atom → a
impossible a = unsafeCrashWith $ "Data.Variant: impossible `" <> (show a) <> "`"
