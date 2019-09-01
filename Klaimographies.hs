--
-- Authors: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This module implements Klaimographies

module Klaimographies where

import Data.List as L
import Data.Set as S
import Data.Maybe
import Misc(intersect)
import Data.Map.Strict as M
import System.FilePath.Posix
import qualified Data.Text as T

type Var = String
type Loc = String
type Subst a = Maybe (Map Var a)

emptySubst :: Subst a
emptySubst = Just M.empty

-- ell ranges over Var union Loc

data Sort = IntNum
  | Str
  | Bool
  | Locality
  deriving (Eq,Ord,Show)

data TupleType = Star
  | Sort
  | NamedSort Var Sort
  | FreshNamedSort Var Sort
  | Tuple [TupleType] 
  deriving (Eq,Ord,Show)

extendSubst :: Subst a -> Subst a -> Subst a
extendSubst s s' =
  case (s, s') of
    (Just sigma, Just sigma') ->
      if Misc.intersect (S.fromList $ M.keys sigma) (S.fromList $ M.keys sigma')
      then Nothing
      else Just (M.union sigma sigma')

fn :: TupleType -> (Subst Sort)
fn tuple = case tuple of
             Star -> emptySubst
             Sort -> emptySubst
             NamedSort x s -> Just (M.fromList [(x,s)])
             FreshNamedSort x s -> emptySubst
             Tuple tuples -> L.foldr extendSubst emptySubst (L.map fn tuples)
  
dn :: TupleType -> Subst Sort
dn tuple = case tuple of
             Star -> emptySubst
             Sort -> emptySubst
             NamedSort x s -> emptySubst
             FreshNamedSort x s -> Just (M.fromList [(x,s)])
             Tuple tuples -> L.foldr extendSubst emptySubst (L.map dn tuples)

             
tuplematch :: TupleType -> TupleType -> Subst Sort
tuplematch tuple tuple' =
  case (tuple, tuple') of
    (_, Star) -> emptySubst
    (Star, _) -> emptySubst
    (Sort, Sort) -> emptySubst
    (Sort, NamedSort _ _) -> emptySubst
    (NamedSort _ _, Sort) -> emptySubst
    (Tuple (t:ts), Tuple (t':ts')) ->
      extendSubst (tuplematch t t') (tuplematch (Tuple ts) (Tuple ts'))
    


applySubst :: Subst Var -> TupleType -> TupleType
applySubst subs tuple =
  case (subs, tuple) of
    (Nothing, _) -> error("Bad substitution")
    (Just sigma, NamedSort x s) ->
      if (L.elem x (M.keys sigma))
      then NamedSort (sigma!x) s
      else tuple
    (Just sigma, Tuple t) -> Tuple (L.map (applySubst subs) t)
    _ -> tuple
