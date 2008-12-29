{-| Analysis and design of this module

If there are SCCs then additional files are inevitable.
Separate files to define the keys are usually avoidable.
Separating Key definitions are needed only if the key imports form a SCC,
and even these may be sometimes avoided by declaring keys in boot files.

Choose to minimize the create of separate Key-desf definitions by
locating and breaking the key SCCs first.  Use the "score" to pick a
single vertex at a time to greedily minimize the number of separate
Key-defs.  After this initial step the set of separate Key-defs is
fixed.

With this understanding, there are FOUR renderings of a descriptor.
Two renderings have non-separate Key-defs (or no Key-defs at all).
Two renderings have separate Key-defs.

The two non-separate Key-defs renderings are:

"simple" :
 *  normal file with the type-def and any Key-defs

"type-boot" :
 *  normal file with the type-def and any Key-defs
 -  hs-boot file declares the type-def

The other two have separate Key-defs, in ".hs-boot" or "'Key.hs" files:

"key-type-boot" :
 +  normal file with the type-def and the Key-defs
 *  hs-boot file declares the type-def and the Key-defs

"split-key,type-boot" :
 +  normal file with the type-def and maybe imports keyfile
 *  keyfile file with the the Key-defs
 -  hs-boot file declares the type-def

In general, all nodes without keys could be rendered as "type-boot"
and all nodes with keys as "split-key,type-boot", which would break
all SCCs. But that is wasteful: 2 or 3 files each (2 for root
ProtoInfo file).  And this waste will also lead to warnings from ghc
nagging about unneeded {-# SOURCE #-} pragmas.

Only the files marked with * have incoming and outgoing edges and NEED
to be considered.  With enough {-# SOURCE #-} pragmas, the + are
just sources and - are just sinks.

Initially all renderings are optimistically Simple.  Some are quickly
changed into TypeBoot by observing the modules which import foreign
keys and marking the reciprocal type imports as TypeBoot.

The next task is to break the SCCs which arise just from the foreign
key imports.  The algorithm makes a graph of these and breaks all of
them by changing the one with the best score from a TypeBoot node into
a KeyTypeBoot node.

Note: The top protoInfo node will be rendered like "simple" as
TopProtoInfo and never change.  The most that happens to the top
protoInfo node is that its targets get changed and some imports get
SOURCE pragmas.

Now considering both type and key imports as links, more SCCs
might arise.  These are also scored. The thing to grasp is how
changing a message's rending is allowed to happen:

TopProtoInfo will never change
Simple may become TypeBoot
TypeBoot will never change
KeyTypeBoot may become SplitKeyTypeBoot
SplitKeyTypeBoot will never change

It always possible to choose a vertex in any SCC that can change,
which is not obvious.  The deduction is that if all vertices in the
SCC are unchanging then there are no internal type import links; thus
the only loops being created are with foreign key imports.  The
initial setup broke all SCCs made only of foreign key imports; thus
this stuck SCC is a contradiction.

The best score is the choice that reduces the size of the scc in the
next round (and secondarily increases the number of sub-SCCs).

The final Result is a Map of names to the non-Simple/TopProtoInfo
renderings and a list of "pairs" (a,p,b) where part 'p' of module 'a'
should import the type defined in 'b' using a SOURCE pragma.  Keys
from messages rendered as KeyTypeBoot should be imported using SOURCE
pragmas.  Keys from messages rendered as SplitKeyTypeBoot should be
imported from the auxiliary 'Key files.

The code below is more complicated in order to reduce the SOURCE
pragmas and avoid ghc's warnings.  All files are tracked and SOURCE
pragmas are added in steps.  This is unlikely to be perfect — some
extra SOURCE pragmas might be left over, but I do not have an example
of this happening.  This also means a DescriptorInfo may have several
file parts and these may end up in different SCCs.  To simplify
processing these different SCCs which share a DescriptorInfo are
merged by 'rejoinVertices'.

-}

module Text.ProtocolBuffers.ProtoCompile.BreakRecursion
  ( makeResult,displayResult,Result(..),VertexKind(..),Part(..),pKey,pfKey,getKind ) where

import Prelude hiding (pi)
import Control.Monad(guard,mplus)
import qualified Data.Foldable as F
import Data.Function(on)
import Data.Graph
import Data.List
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(mapMaybe)
import Data.Monoid
import qualified Data.Set as Set
import Data.Set(Set)
import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Identifiers
import Text.ProtocolBuffers.Reflections

import Debug.Trace(trace)

ecart :: String -> a -> a
ecart _ a = a

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

imp :: String -> a
imp s = error $ "Inconceivable! Text.ProtocolBuffers.ProtoCompile.BreakRecursion."++s

iguard :: Monad m => Bool -> String -> m ()
iguard True _ = return ()
iguard False s = imp s

-- The Gen.hs module will be working with these String types
type MKey = FMName String

pKey :: ProtoName -> MKey
pKey (ProtoName {haskellPrefix=a,parentModule=b,baseName=c}) = foldr1 dotFM . map promoteFM $ a++b++[c]

pfKey :: ProtoFName -> MKey
pfKey (ProtoFName {haskellPrefix'=a,parentModule'=b}) = foldr1 dotFM . map promoteFM $ a++b

-- Which reprensentation a message currently has
data VertexKind = TopProtoInfo
                | Simple
                | TypeBoot
                | KeyTypeBoot
                | SplitKeyTypeBoot
  deriving (Show,Eq,Ord)

-- Which of the 3 sorts of files (maked with * in analysis) a vertex represents
data Part = Normal | Source | KeyFile deriving (Show,Eq,Ord)

-- Vertex data.  A graph may have several nodes with the same value of
-- V and different values of Part.
data V = V { vMKey :: !MKey
           , vNeedsKeys :: !(Set MKey)
           , vKeysNeedsTypes :: !(Set MKey)
           , vTypeNeedsTypes :: !(Set MKey) }
  deriving Show

-- A link to a module's file
data Label = L !Part !MKey deriving (Show,Eq,Ord)

type E = (V,Label,[Label])
type G = [E]
type SCCs = [G]

-- The end product of this module is the Result value
data Result = Result { rKind :: Map MKey VertexKind
                     , rIBoot :: Set (MKey,Part,MKey)
                     , rIKey :: Set (MKey,MKey) }
  deriving Eq

displayResult :: Result -> String
displayResult (Result {rKind = kv, rIBoot = ab, rIKey=ab'keys }) = unlines $
  [ "--- displayResult ----"
  , "Modules which are not Simple"
  ] ++ map (\(k,v) -> indent . shows (fmName k) . ("  has kind  "++) $ show v) (Map.assocs kv) ++
  [ "Module imports marked with SOURCE for Types"
  ] ++ map (indent . untriple) (Set.toAscList ab) ++
  [ "Module imports marked with SOURCE or 'Key for keys"
  ] ++ map (indent . unpair) (Set.toAscList ab'keys)
 where indent = (' ':).(' ':)
       untriple (a,p,b) = fmName a ++ " " ++ show p  ++ " : import {-# SOURCE #-} " ++ fmName b
       unpair (a,b) = fmName a ++ " : import {-# SOURCE or 'Key #-} " ++ fmName b

showSCCs :: SCCs -> String
showSCCs gs = concatMap (\ g -> "\n>< SCC Graph ><\n"++concatMap showE g) gs
showG :: G -> String
showG g = '\n':concatMap showE g
showE :: E -> String
showE (v,n,ls) = unlines $ [ "( "++show n, "  , "++show v, "  , "++show ls, ")" ]

instance Monoid Result where
  mempty = Result mempty mempty mempty
  mappend r1 r2 = Result { rKind = Map.unionWith max (rKind r1) (rKind r2)
                         , rIBoot = mappend (rIBoot r1) (rIBoot r2)
                         , rIKey = mappend (rIKey r1) (rIKey r2) }

getKind :: Result -> MKey -> VertexKind
getKind r = let m = rKind r in \n -> Map.findWithDefault Simple n m

getType :: VertexKind -> Part
getType TopProtoInfo = imp "getType: TopProtoInfo"
getType Simple = Normal
getType TypeBoot = Source
getType KeyTypeBoot = Source
getType SplitKeyTypeBoot = Source

getKey :: VertexKind -> Part
getKey TopProtoInfo = Normal
getKey Simple = Normal
getKey TypeBoot = Normal
getKey KeyTypeBoot = Source
getKey SplitKeyTypeBoot = KeyFile

-- 'makeResult' is the main function for this module
makeResult :: ProtoInfo -> Result
makeResult protoInfo =
  let pvs@(p,vs) = makeVertices protoInfo
      initResult = breakKeys pvs
      sccs = cycles (makeG (p:vs) initResult)
      answer = cull (p:vs) $ breakGraph initResult sccs
      finalGraph = makeG (p:vs) answer
      remainingProblems = cycles finalGraph
      msg = unlines [ "<!!!!!!!!!!!> KLAXON, RED SPINNING LIGHT, ETC."
                    , "! WARNING: hprotoc unexpectedly failed to disentangle all the mutually-recursive message definitions."
                    , "! PLEASE REPORT THIS FAILURE ALONG WITH THE PROTO FILE."
                    , "! The failed subset is:"
                    ] ++ showSCCs remainingProblems ++ "\n</!!!!!!!!!!!>"
  in if null remainingProblems then ecart (showG finalGraph) answer
       else trace msg answer

-- Build the graph using the vertices and the Result so far.
makeG :: [V] -> Result -> G
makeG vs r = concatMap (makeEdgesForV r) vs

-- Returns all as Simple and Normal.  The fst V is from the ProtoInfo
-- the snd [V] is from the DescriptorInfo.
makeVertices :: ProtoInfo -> (V,[V])
makeVertices pi = answer where
  answer =  ( protoInfoV , map makeV (messages pi) )

  protoInfoV = V { vMKey = pKey (protoMod pi)
                 , vNeedsKeys = mempty
                 , vKeysNeedsTypes = knt (extensionKeys pi)
                 , vTypeNeedsTypes = mempty }

  makeV di = V { vMKey = pKey (descName di)
               , vNeedsKeys = nk (knownKeys di)
               , vKeysNeedsTypes = knt (keys di)
               , vTypeNeedsTypes = tnt (fields di) }

  allK = Set.fromList (pKey (protoMod pi) : map (pKey . descName) (messages pi))
  allT = Set.fromList (map (pKey . descName) (messages pi))

  tnt :: Seq FieldInfo -> Set MKey
  tnt fs = Set.intersection allT $ Set.fromList $ map pKey . mapMaybe typeName . F.toList $ fs

  knt :: Seq KeyInfo -> Set MKey
  knt ks =
    let (pns, fsL) = unzip (F.toList ks)
        fnt :: [FieldInfo] -> Set MKey
        fnt fs = Set.fromList $ (map pKey . mapMaybe typeName $ fs) ++ (map (pfKey . fieldName) fs)
    in Set.intersection allT $ Set.union (Set.fromList (map pKey pns)) (fnt fsL)

  nk :: Seq FieldInfo -> Set MKey
  nk fs = Set.intersection allK $ Set.fromList $ map (pfKey . fieldName) . F.toList $ fs

-- The only need for KeyTypeBoot (and SplitKeyTypeBoot) is to break
-- key-only import cycles. 'breakKeys' finds and breaks these SSCs by
-- marking files as KeyTypeBoot.  Since foreign keys implies a
-- reciprocal type import, additional files can get changed to
-- TypeBoot and some incoming links marked to use Source.
breakKeys :: (V,[V]) -> Result
breakKeys (pv,vsOther) =
  let vs = pv : vsOther
      es = map makeInitialEdges vs where
        makeInitialEdges v = (v,L Normal self,[ L Normal b | b <- Set.toList (vNeedsKeys v), b/=self ])
          where self = vMKey v
      -- For 'a'/='b': if 'a' needs key from 'b' then 'b' must need type from 'a'
      -- this recursion means 'a' cannot be Simple so change to TypeBoot
      startingResult = Result { rKind = needTypeBoot, rIBoot = mempty, rIKey = mempty }
      needTypeBoot = Map.singleton (vMKey pv) TopProtoInfo `Map.union`
                     ( Map.fromList . map (\(_,L _ a,_) -> (a,TypeBoot))
                                    . filter (\(_,_,bLs) -> not (null bLs)) $ es )
      -- break always moves things to KeyTypeBoot from TypeBoot (not
      -- Simple) because they are in a Key-import SCC: this means they
      -- are importing foreign keys and thus they are in needTypeBoot
      breakSCCs :: Result -> SCCs -> Result
      breakSCCs r sccs = r `mappend` mconcat (map breakSCC sccs)
      breakSCC :: G -> Result
      breakSCC [] = imp $ "breakKeys.breakSCC: The SCC cannot be empty!"
      breakSCC es' = let (toBust,next'sccs) = snd $ maximumBy (compare `on` fst) (map f (pullEach es'))
                           where f ((v,_,_),es'') = let (s,sccs) = score es'' in (s,(v,sccs))
                         bk = vMKey toBust -- ZZZ 
                         ik = Set.fromList [ (ek,bk) | ek <- map (vMKey . fst3) es', ek/=bk ] -- ZZZ
                         newResult = Result { rKind = Map.singleton bk KeyTypeBoot
                                            , rIBoot = mempty
                                            , rIKey = ik } -- ZZZ
                     in breakSCCs newResult next'sccs
      -- Init boot marks some incoming links to use SOURCE
      initBoot r = r { rIBoot = Set.fromList . concatMap withParts $ es }
        where withParts (_,L _ a,bLs) = [ withPart a b | L _ b <- bLs ]
              withPart a b = let p = getKey (getKind r b) in (b,p,a)
  in initBoot $ breakSCCs startingResult (cycles es)

score :: G -> ( (Int,Int), SCCs )
score es = ((value,parts),sccs) where
  sccs = cycles es
  -- A length n SCC can be solved by changing at most (n-1) vertices
  -- The value is the difference between the
  --   old graph which required at most (pred . length) ed changes
  --   and the new graphs which require at most (sum (map (pred . length) sccs)) changes
  -- so a larger value is preferred
  value = (pred . length) es - (sum (map (pred . length) sccs))
  -- The number of parts is used as a potential tie breaker, prefering more parts
  parts = length sccs -- # of pieces

-- select the non-trivial sccs from edges
cycles :: G -> SCCs
cycles = filter atLeastTwo . map flattenSCC . stronglyConnCompR
  where atLeastTwo :: [a] -> Bool
        atLeastTwo (_:_:_) = True
        atLeastTwo _ = False

-- pull out each element as candidate and list without the element
pullEach :: [a] -> [(a,[a])]
pullEach = go id where go _ [] = []
                       go f (x:xs) = (x,f xs) : go (f . (x:)) xs

-- This builds an edge E from the vertex V and ensures that V has the
-- right vKind from the Result.  This must make the same judgements as
-- Gen.hs does in importPN and import PFN
makeEdgesForV :: Result -> V -> [E]
makeEdgesForV r v =
  let me = vMKey v;  myKind = getK me
      getK = getKind r;  self p = L p me;
      typeL p n = if Set.notMember (me,p,n) (rIBoot r) then L Normal n
                    else let checkSource = getType (getK n)
                         in if checkSource == Source then L Source n  -- sanity check
                              else error "makeEdgesForV.typeL.getType.getK of n did not return Source!"
      keyL n = if Set.notMember (me,n) (rIKey r) then L Normal n
                 else L (getKey (getK n)) n
      sKNT (L p _) = Set.map (typeL p) (vKeysNeedsTypes v)
      sTNT (L p _) = Set.map (typeL p) (vTypeNeedsTypes v)
      sNK _ = Set.map keyL (vNeedsKeys v)

      notMe set = [ e | e@(L _p o) <- Set.toList set, o/=me ]
      standard = let s = self Normal in (v,s,notMe $ Set.unions [ sKNT s, sTNT s, sNK s])
      source = let s = self Source in (v,s,[])
      sourceKTB = let s = self Source in (v,s,notMe $ sKNT s)
      standardSKTB = let s = self Normal in (v,s,notMe' $ Set.union (sNK s) (sTNT s))
        where notMe' set = [ e | e@(L p o) <- Set.toList set, o/=me || p==KeyFile ]
      keyfileSKTB = let s = self KeyFile in (v,s,Set.toList $ sKNT s)

  in case myKind of -- commented out the purely SOURCE and SINK nodes:
       TopProtoInfo     -> [standard]
       Simple           -> [standard]
       TypeBoot         -> [standard,source]
       KeyTypeBoot      -> [standard,sourceKTB]
       SplitKeyTypeBoot -> [standardSKTB,keyfileSKTB,source]

breakGraph ::  Result -> SCCs -> Result
breakGraph r [] = ecart ("\nbreakGraph leaf answer\n"++displayResult r) $ r
breakGraph r sccs = ecart ("\nbreakGraph\n"++displayResult r) $
                    r `mappend` mconcat (map (breakCycle r) (rejoinVertices sccs))

-- I wonder if there is any input which leads to a module having
-- different parts in different SCCs.  Rather than try and
-- over-analyze this wierd edge case this 'rejoinVertices' function
-- will detect it and join the SCCs.
rejoinVertices :: SCCs -> SCCs
rejoinVertices [] = []
rejoinVertices g@([_]) = g
rejoinVertices gs = 
  let vgs :: [(Set MKey,G)]
      vgs = map (\ g -> (Set.fromList . map (vMKey . fst3) $ g,g)) gs
      process [] = []
      process ((_,g):[]) = [g]
      process ((v,g):rest) = walk id rest where
        walk p [] = g : process (p [])
        walk p (x@(v',g'):rest') | Set.null (Set.intersection v v') = walk (p . (x:)) rest'
                                 | otherwise = process ((Set.union v v',g++g') : p [])
  in process vgs

{-
breakCycle is a work in progress.  The ans' value tries to change
incoming type links to the Source file, then ans'R. The ans'R tries to
change outgoing links to point to Source files.  The ans'TB changes
from Simple/Normal to TypeBoot (adding a source file).  The ans'SKTB
changes from KeyTypeBoot/Source to SplitKeyTypeBoot.

The reason these changes are done in stages is to try and avoid ghc's
warnings that a {-# SOURCE #-} import is not not needed.
-}
breakCycle :: Result -> G -> Result
breakCycle oldR sccIn = 
  let bits = map snd3 sccIn -- trace
      -- toCompare should never be null.
      toCompare = mapMaybe f (pullEach sccIn) where
        allV = Set.fromList (map (vMKey . fst3) sccIn)
        f :: (E,[E]) -> Maybe ((Int, Int), (Result, SCCs))
        f (e@(v,L p me,_bLs), es) = ecart (">< picking:\n"++showE e++
                                           "\nfrom:"++show bits++
                                           "\nscore: "++show observe++"\n") $
                                    answer where
          answer = case (getKind oldR me,p) of
                     (TopProtoInfo,Normal)      -> ans'R
                     (Simple,Normal)            -> ans'R  `mplus` ans'TB -- ans' is part of ans'TB
                     (TypeBoot,Normal)          -> ans'   `mplus` ans'R
                     (KeyTypeBoot,Normal)       -> ans'   `mplus` ans'R
                     (KeyTypeBoot,Source)       -> ans'RK `mplus` ans'SKTB -- ans' may be redundant
                     (SplitKeyTypeBoot,Normal)  -> ans'   `mplus` ans'R
                     (SplitKeyTypeBoot,KeyFile) -> ans'RK -- ans' may be redundant
                     (TypeBoot,Source) -> imp $
                       "breakCycle.toCompare.f cannot have (TypeBoot,Source) in SCC!" ++ eMsg
                     (SplitKeyTypeBoot,Source) -> imp $
                       "breakCycle.toCompare.f cannot have (SplitKeyTypeBoot,Source) in SCC!" ++ eMsg
                     _ -> imp $ "breakCycle.toCompare.f: impossible combination in SCC:"++ eMsg
          observe = case answer of Nothing -> "Nothing"; Just (s,_) -> "Just "++show s  -- trace
          eMsg = '\n':unlines (map showE (e:es))

          ans',ans'R,ans'TB,ans'SKTB :: Maybe ((Int, Int), (Result, SCCs))
          ans' = if Set.null newIBoot then Nothing
                   else go $ oldR `mappend` Result { rKind = mempty
                                                   , rIBoot = newIBoot
                                                   , rIKey = mempty }
          ans'R = if Set.null newIBootR then Nothing
                    else go $ oldR `mappend` Result { rKind = mempty
                                                    , rIBoot = newIBootR
                                                    , rIKey = mempty }
          ans'RK = if Set.null newIBootRK then Nothing
                     else go $ oldR `mappend` Result { rKind = mempty
                                                     , rIBoot = newIBootRK
                                                     , rIKey = mempty }
          ans'TB = go $ oldR `mappend` Result { rKind = Map.singleton me TypeBoot
                                              , rIBoot = newIBoot -- do (TypeBoot,Normal) -> ans'
                                              , rIKey = mempty }
          ans'SKTB = go $ oldR `mappend` Result { rKind = Map.singleton me SplitKeyTypeBoot
                                                , rIBoot = newIBootSKTB
                                                , rIKey = Set.singleton (me,me) }

          newIBoot,newIBootR,newIBootRK,newIBootSKTB :: Set (MKey,Part,MKey)
          newIBoot = Set.fromList $ do
            (va,L pa a,_) <- es
            iguard (Set.member a allV) $
              "breakCycle.toCompare.newIBoot sanity check 083425 failed:"++eMsg
            guard (((pa == Normal) &&
                    (Set.member me (vTypeNeedsTypes va))) ||
                   ((pa == getKey (getKind oldR a)) &&
                    (Set.member me (vKeysNeedsTypes va))))
            let x=(a,pa,me)
            guard (Set.notMember x (rIBoot oldR))  -- needed when used in newIBoot2
            return x
          newIBootR = Set.fromList $ do
            b <- Set.toList (Set.union (vTypeNeedsTypes v) (vKeysNeedsTypes v))
            guard (Set.member b allV)
            guard (Source == getType (getKind oldR b))
            guard (me /= b || p == KeyFile)
            let x = (me,p,b)
            guard (Set.notMember x (rIBoot oldR))
            return x
          newIBootRK = Set.fromList $ do
            b <- Set.toList (vKeysNeedsTypes v)
            guard (Set.member b allV)
            guard (Source == getType (getKind oldR b))
            guard (me /= b || p == KeyFile)
            let x = (me,p,b)
            guard (Set.notMember x (rIBoot oldR))
            return x
          newIBootSKTB = Set.union newIBoot . Set.fromList $ 
            (if Set.member me (vKeysNeedsTypes v) then ((me,KeyFile,me):) else id) $ do
              b <- Set.toList (vKeysNeedsTypes v)
              guard (Set.member (me,Source,b) (rIBoot oldR)) -- copy from (me,Source,b)
              let x = (me,KeyFile,b)                         -- copy to (me,KeyFile,b)
              iguard (Set.notMember x (rIBoot oldR)) $
                "breakCycle.toCompare.newIBoot2 KeyTypeBoot already had entries for KeyFile!:"++eMsg
              return x

          go :: Result -> Maybe ((Int, Int), (Result, SCCs))
          go newR = let (s,sccs) = score (makeG (map fst3 (e:es)) newR)
                    in Just (s,(newR,sccs))
  in ecart (">< breakCycle of "++show bits++"\n\n") $
     if null toCompare
       then imp $ "breakCycle: This SCC had no Simple or KeyTypeBoot nodes!\n"++ unlines (map show sccIn)
       else let (newR,next'sccs) = snd $ maximumBy (compare `on` fst) toCompare
            in breakGraph newR next'sccs

-- 'cull' tries to remove all the extra {-# SOURCE #-} pragmas.  I am
-- not certain that repeating the 'cull' will make any difference.
cull :: [V] -> Result -> Result
cull vs rIn =
  let trial :: Result -> (MKey,Part,MKey) -> Result
      trial old x = let new = old { rIBoot = Set.delete x (rIBoot old) }
                    in if null (cycles (makeG vs new)) then new else old
      rOut = foldl' trial rIn (Set.toList (rIBoot rIn))
  in if rOut == rIn then rOut else cull vs rOut

