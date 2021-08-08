--
-- Author: Emilio Tuosto <emilio.tuosto@gssi.it>
--
-- This module contains operations on c-automata.
--

module ChorAutomata where

import CFSM (Channel)
import Data.Set as S
import Data.List as L
import Data.Map.Strict as M
import Misc
import DotStuff
import qualified Data.Text as T

data Interaction =
  Comm Channel Message
  | Tau
  deriving (Ord, Eq, Show)

type CAutomaton = Graph Int Interaction

trxClosure :: (Interaction -> Bool) -> CAutomaton -> CAutomaton
trxClosure cond (states, q0, ls, trxs) =
  let
    closure q =
      let
        tgt = \(_, _, t) -> t
        step = S.map tgt (S.filter (\(q', l, _) -> q' == q && (cond l)) trxs)
      in
        S.union (S.singleton q) (S.foldr S.union step (S.map closure step))
    states' = S.map closure states
    trxsCond = M.fromList [(q, S.filter (\(s,l,t) -> s==q && not(cond l)) trxs) | q <- S.toList states]
    aux cl (q,l,q') = (cl, l, S.findIndex (closure q') states')
    newTr qs = S.map (aux (S.findIndex qs states')) (S.unions $ S.map (\q -> trxsCond!q) qs)
    trxs' = S.unions $ S.map newTr states'
  in
    (S.map (\q -> S.findIndex q states') states', (S.findIndex (closure q0) states'), ls, trxs')

interaction2string :: Interaction -> String
interaction2string Tau = "tau"
interaction2string (Comm (s,r) l) = (show s) ++ " -> " ++ (show r) ++ ": " ++ (show l)

parseCA :: String -> CAutomaton
parseCA text =
  --
  -- parseCA returns a system provided that 'text' represents a CA
  -- according to the following syntax:
  --     C ::= Int I Int
  --        |  C NewLine C
  --     I ::= Str -> Str : Str
  --        |  Str -> Str Str
  --        |  Str Str Str
  --        |  Str Str : Str
  --        |  tau
  --
  -- where Int is an integer, Str is a string, and NewLine is the end
  -- of line token.  Lines starting with '--' are interpreted as
  -- comments and ignored. The first line must be the first transition
  -- of the initial state.
  --
  parsing 1 lines (S.empty, q0, S.empty, S.empty)
  where
    lines = L.map Prelude.words (Prelude.lines text)
    q0 = if lines == []
      then error "parseCA: there should be at least a state"
      else
      if head lines == []
      then error "parseCA: there should be at least a state"
      else read (head $ head lines)::Int
    parsing i t ca@(qs, q0, ls, ts) =
      case t of
        l:xs ->
          case l of
            [] -> parsing (i+1) xs ca
            _ ->
              case head l of
                '-':w ->
                  if head w == '-'
                  then parsing (i+1) xs ca
                  else error ("parseCA: syntax error on line " ++ (show i))
                _ -> 
                  case l of
                    [q, s, r, m, q'] ->
                      let
                        source = (read q)::Int
                        target = (read q')::Int
                        label = (Comm (s,r) m)
                        ca' =
                          (S.union qs (S.fromList [source, target]),
                           q0,
                           S.union ls (S.singleton label),
                           S.union ts (S.singleton (source, label, target))
                          )
                      in
                        parsing (i+1) xs ca'
                    [q, s, "->", r, m, q'] ->
                      let
                        source = (read q)::Int
                        target = (read q')::Int
                        label = (Comm (s,r) m)
                        ca' =
                          (S.union qs (S.fromList [source, target]),
                           q0,
                           S.union ls (S.singleton label),
                           S.union ts (S.singleton (source, label, target))
                          )
                      in
                        parsing (i+1) xs ca'
                    [q, s, r, ":", m, q'] ->
                      let
                        source = (read q)::Int
                        target = (read q')::Int
                        label = (Comm (s,r) m)
                        ca' =
                          (S.union qs (S.fromList [source, target]),
                           q0,
                           S.union ls (S.singleton label),
                           S.union ts (S.singleton (source, label, target))
                          )
                      in
                        parsing (i+1) xs ca'
                    [q, s, "->", r, ":", m, q'] ->
                      let
                        source = (read q)::Int
                        target = (read q')::Int
                        label = (Comm (s,r) m)
                        ca' =
                          (S.union qs (S.fromList [source, target]),
                           q0,
                           S.union ls (S.singleton label),
                           S.union ts (S.singleton (source, label, target))
                          )
                      in
                        parsing (i+1) xs ca'
                    [q,"tau",q'] ->
                      parsing (i+1) xs ca'
                      where
                        ca' =
                          (S.union qs (S.fromList [(read q)::Int, (read q')::Int]),
                           q0,
                           S.union ls (S.singleton Tau),
                           S.union ts (S.singleton ((read q)::Int, Tau, (read q')::Int))
                          )
                    _ ->
                      error ("parseCA: syntax error on line " ++ (show i))
        _ -> ca


ca2dot :: CAutomaton -> String -> Map String String -> String
ca2dot ( states, q0, _, trxs ) name flines =
  "digraph " ++ name ++ " {\n\tlabel=\"" ++ name ++ "\"\n" ++ pstates ++ ptx ++ "\n}"
  where pstates =
          "\t" ++ (show q0) ++
          "\t[style=" ++ (flines!q0style) ++
          ", color=" ++ (flines!q0col) ++ "]\n" ++
          S.foldr (++) ""
            (S.map (\x ->
                      "\t" ++ (show x) ++
                         "\t[label = \"" ++
                          (show x) ++
                         "\"];\n"
                   )
              states
            )
        ptx =
          S.foldr (++) "" (
            S.map (\( s, int, t ) -> 
                  "\t" ++ (show s)
                  ++ " -> " ++
                  (show t)
                  ++
                  "\t[label = \"" ++ (rmChar '\"' $ interaction2string int) ++ "\"];\n"
            )
            trxs
          )
