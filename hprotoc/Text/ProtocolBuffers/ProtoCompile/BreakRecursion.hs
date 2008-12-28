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

The two non-sepatate Key-defs renderings are

"simple" :
 *  normal file with the type-def and any Key-defs

"type-boot" :
 *  normal file with the type-def and any Key-defs
 -  hs-boot file declares the type-def

The second pair do not care whether the Key-defs use the type:

"key-type-boot" :
 +  normal file with the type-def and the Key-defs
 *  hs-boot file declares the type-def and the Key-defs

"split-key,type-boot" :
 +  normal file with the type-def and maybe imports keyfile
 *  keyfile file with the the Key-defs
 -  hs-boot file declares the type-def

In general, all nodes without keys could be rendered as "type-boot"
and all nodes with keys as "split-key,type-boot", which would break all
SCCs. But that is wasteful: 2 or 3 files each (2 for root ProtoInfo file).

Only the files marked with * have incoming and outgoing edges and need
to be considered.  The + are just sources and - are just sinks.

Initially all renderings are optimistically Simple.  Some are quickly
changed into TypeBoot by observing the modules which import foreign
keys and marking the reciprocal type imports as TypeBoot.

The next task is to break the SCCs which arise just from the foriegn
key imports.  The algorithm makes a graph of these and breaks all of
them by changing the one with the best score from a TypeBoot node into
a KeyTypeBoot node.

Note: The top protoInfo node will be rendered like "simple" as
TopProtoInfo and never change.  The most that happens to the top
protoInfo node is that its targets get changed and some imports get
SOURCE pragmas.

Now considering both type and key imports as links, some more SCCs
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
renderings and a list of pairs (a,b) where 'a' should import the type
defined in 'b' using a SOURCE pragma.  Keys from messages rendered as
KeyTypeBoot should be imported using SOURCE pragmas.  Keys from
messages rendered as SplitKeyTypeBoot should be imported from the
auxiliary 'Key files.

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
data VertexKind =
         TopProtoInfo
       | Simple
       | TypeBoot
       | KeyTypeBoot
       | SplitKeyTypeBoot
  deriving (Show,Eq,Ord)

-- Which of the 3 sorts of files (maked with * in analysis) a vertex represents
data Part = Normal | Source | KeyFile deriving (Show,Eq,Ord)

-- A vertex. Note that the value vPart is restricted by the value of vKind.
data V = V { vMKey :: !MKey
           , vNeedsKeys :: !(Set MKey)
           , vKeysNeedsTypes :: !(Set MKey)
           , vTypeNeedsTypes :: !(Set MKey) }
  deriving Show

-- An edge to a module's file
data Label = L !Part !MKey deriving (Show,Eq,Ord)

type E = (V,Label,[Label])
type G = [E]
type SCCs = [G]

data Result = Result { rKind :: Map MKey VertexKind
                     , rIBoot :: Set (MKey,Part,MKey)
                     , rIKey :: Set (MKey,MKey) }

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

makeResult :: ProtoInfo -> Result
makeResult pi =
  let pvs@(p,vs) = makeVertices pi
      result1 = breakKeys pvs
      graph1 = makeG (p:vs) result1
      answer = breakGraph result1 (cycles graph1)
      msg1 = makeG (p:vs) answer
      msg2 = cycles msg1
  in trace ("makeResult's final graph is:"++showG msg1++showSCCs msg2) $
     answer

showSCCs :: SCCs -> String
showSCCs gs = concatMap (\ g -> "\n>< SCC Graph ><\n"++concatMap showE g) gs

showG :: G -> String
showG g = '\n':concatMap showE g

showE :: E -> String
showE (v,n,ls) = unlines $ [ "( "++show n
                           , "  , "++show v
                           , "  , "++show ls
                           , ")"
                           ]

-- Returns all as Simple and Normal
makeVertices :: ProtoInfo -> (V,[V])
makeVertices pi =
  let answer =  ( protoInfoV , map makeV (messages pi) )
      protoInfoV = V { vMKey = pKey (protoMod pi)
                     , vNeedsKeys = mempty
                     , vKeysNeedsTypes = knt (extensionKeys pi)
                     , vTypeNeedsTypes = mempty }
      makeV di = V { vMKey = pKey (descName di)
                   , vNeedsKeys = nk (knownKeys di)      -- might include self
                   , vKeysNeedsTypes = knt (keys di)     -- might include self
                   , vTypeNeedsTypes = tnt (fields di) } -- might include self
      allVT = Set.fromList (map (pKey . descName) (messages pi))
      allV = Set.fromList (pKey (protoMod pi) : map (pKey . descName) (messages pi))
      knt :: Seq KeyInfo -> Set MKey
      knt ks =
        let (pns, fsL) = unzip (F.toList ks)
            fnt :: [FieldInfo] -> Set MKey
            fnt fs = Set.fromList $ (map pKey . mapMaybe typeName $ fs) ++ (map (pfKey . fieldName) fs)
        in Set.intersection allVT $ Set.union (Set.fromList (map pKey pns)) (fnt fsL)

      nk :: Seq FieldInfo -> Set MKey
      nk fs = Set.intersection allV $ Set.fromList $ map (pfKey . fieldName) . F.toList $ fs

      tnt :: Seq FieldInfo -> Set MKey
      tnt fs = Set.intersection allVT $ Set.fromList $ map pKey . mapMaybe typeName . F.toList $ fs
  in answer

-- The only need for KeyTypeBoot and SplitKeyTypeBoot is to break
-- key-only import cycles.  'breakKeys' find and breaks these SSCs.
-- It will also mark files as TypeBoot if they improt foreign keys,
-- and add the reciprocal link to rIBoot.
breakKeys :: (V,[V]) -> Result
breakKeys (pv,vsOther) =
  let vs = pv : vsOther
      es = map makeInitialEdges vs where
        makeInitialEdges v = (v,L Normal self,[ L Normal b | b <- Set.toList (vNeedsKeys v), b/=self ])
          where self = vMKey v
      -- For 'a'/='b': if 'a' needs key from 'b' then 'b' must need type from 'a'
      -- this recursion means 'a' cannot be Simple so change to TypeBoot
-- ZZZ      startingResult = Result { rKind = needTypeBoot, rIBoot = reversed }
-- ZZZZ      startingResult = Result { rKind = needTypeBoot, rIBoot = reversed, rIKey = mempty } -- ZZZ
      startingResult = Result { rKind = needTypeBoot, rIBoot = mempty, rIKey = mempty } -- ZZZ
      needTypeBoot = Map.singleton (vMKey pv) TopProtoInfo `Map.union`
                     ( Map.fromList . map (\(_,L _ a,_) -> (a,TypeBoot))
                                    . filter (\(_,_,bLs) -> not (null bLs)) $ es )
      -- module b import a SOURCE a'type
--      reversed = Set.fromList . concatMap (\(_,L _ a,bLs) -> [ (b,a) | L _ b <- bLs ]) $ es
      -- break always moves things to KeyTypeBoot from TypeBoot (not
      -- Simple) because they are in a Key-import SCC: this means they
      -- are importing foreign keys and thus they are in needBoot with
      -- TypeBoot
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

-- pull out each element as candidate
pullEach :: [a] -> [(a,[a])]
pullEach = go id where go _ [] = []
                       go f (x:xs) = (x,f xs) : go (f . (x:)) xs

-- Build the graph using the vertices and the Result so far.
-- The vKind will be taken from the Result, not the vertices.
makeG :: [V] -> Result -> G
makeG vs r = concatMap (makeEdgesForV r) vs

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

{-
      sTypes p = Set.map (typeL p) . Set.delete me -- used to avoid "me"
      notMe = Set.toList (Set.union
                 (Set.map keyL . Set.delete me $ vNeedsKeys v) -- avoid "me"
                 (sTypes p $ Set.union (vKeysNeedsTypes v) (vTypeNeedsTypes v)) ) -- avoid "me"
      fromKTB p = Set.toList (sTypes p (vKeysNeedsTypes v)) -- avoid "me"
      fromT = Set.toList (Set.union
                 (Set.map keyL $ vNeedsKeys v) -- include "me"
                 (sTypes (vTypeNeedsTypes v))) -- avoid "me"
      fromK p = Set.toList (Set.map (typeL p) (vKeysNeedsTypes v)) -- include "me"
-}

breakGraph ::  Result -> SCCs -> Result
breakGraph r [] =  trace ("\nbreakGraph leaf answer\n"++displayResult r) $ r
breakGraph r sccs = trace ("\nbreakGraph\n"++displayResult r) $
                    r `mappend` mconcat (map (breakCycle r) sccs)

{-

breakCycle is a work in progress.  The ans' value tries to change
incoming type links to the Source file, then ans'R. The ans'R tries to
change outgoing links to point to Source files.

(Simple,Normal) can then try to change to (TypeBoot,Normal and Source)
XXX todo: need to add the extra vertex or recompute G!
(KeyTypeBoot,Source) can try to change to (SplitKeyTypeBoot,KeyFile and Source)
XXX todo: need to add the extra vertex or recompute G!

-}
breakCycle :: Result -> G -> Result
breakCycle oldR sccIn = 
  let bits = map snd3 sccIn
      toCompare = mapMaybe f (pullEach sccIn)
        where allV = Set.fromList (map (vMKey . fst3) sccIn)
              f :: (E,[E]) -> Maybe ((Int, Int), (Result, SCCs))
              f (e@(v,L p me,_bLs), es) =
                let ans = case (getKind oldR me,p) of
                            (TopProtoInfo,Normal) -> ans'R
                            (Simple,Normal) -> ans'R `mplus` ans'TB
                            (TypeBoot,Normal) -> ans' `mplus` ans'R
                            (TypeBoot,Source) -> imp $ "breakCycle.toCompare.f cannot have (TypeBoot,Source) in SCC!"
                                                      ++ unlines (map show (e:es))
                            (KeyTypeBoot,Normal) -> ans' `mplus` ans'R
                            (KeyTypeBoot,Source) -> ans'R `mplus` ans'SKTB
                            (SplitKeyTypeBoot,Normal) -> ans' `mplus` ans'R
                            (SplitKeyTypeBoot,Source) -> imp $ "breakCycle.toCompare.f cannot have (SplitKeyTypeBoot,Source) in SCC!"
                                                      ++ unlines (map show (e:es))
                            (SplitKeyTypeBoot,KeyFile) -> ans'R
                            _ -> imp $ "breakCycle.toCompare.f: impossible combination in SCC:"++ eMsg
                    observe = case ans of
                                Nothing -> "Nothing"
                                Just (s,_) -> "Just "++show s
                 in trace (">< picking:\n"++showE e++"\nfrom:"++show bits++"\nscore: "++show observe++"\n") $
                    ans
                where eMsg = '\n':unlines (map showE (e:es))
                      newIBoot = Set.fromList $ do
                                   (va,L pa a,_) <- es
                                   iguard (Set.member a allV) $ "breakCycle.toCompare.newIBoot sanity check 083425 failed:"++eMsg
-- not applicable when used in newIBoot2 -- iguard (a /= me) $ "breakCycle.toCompare.newIBoot Simple vertex in SCC twice:"++eMsg
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
                      newIBoot2 = Set.union newIBoot . Set.fromList $ 
                                  (if Set.member me (vKeysNeedsTypes v) then ((me,KeyFile,me):) else id) $ do
                                    b <- Set.toList (vKeysNeedsTypes v)
-- bad check, this should NOT be here -- guard (Set.member b allV)
                                    guard (Set.member (me,Source,b) (rIBoot oldR))
                                    let x = (me,KeyFile,b)
                                    iguard (Set.notMember x (rIBoot oldR)) $ "breakCycle.toCompare.newIBoot2 KeyTypeBoot already had entries for KeyFile!:"++eMsg
                                    return x
                      ans',ans'R,ans'TB,ans'SKTB :: Maybe ((Int, Int), (Result, SCCs))
                      ans' = if Set.null newIBoot
                               then Nothing
                               else go $ oldR `mappend` Result { rKind = mempty
                                                               , rIBoot = newIBoot
                                                               , rIKey = mempty }
                      ans'R = if Set.null newIBootR
                                then Nothing
                                else go $ oldR `mappend` Result { rKind = mempty
                                                                , rIBoot = newIBootR
                                                                , rIKey = mempty }
                      ans'TB = go $ oldR `mappend` Result { rKind = Map.singleton me TypeBoot
                                                          , rIBoot = newIBoot
                                                          , rIKey = mempty }
                      ans'SKTB = go $ oldR `mappend` Result { rKind = Map.singleton me SplitKeyTypeBoot
                                                            , rIBoot = newIBoot2
                                                            , rIKey = Set.singleton (me,me) }
                      go :: Result -> Maybe ((Int, Int), (Result, SCCs))
                      go newR = let (s,sccs) = score (makeG (map fst3 (e:es)) newR)
                                in Just (s,(newR,sccs))
  in trace (">< breakCycle of "++show bits++"\n\n") $
     if null toCompare
       then imp $ "breakCycle: This SCC had no Simple or KeyTypeBoot nodes!\n"++ unlines (map show sccIn)
       else let (newR,next'sccs) = snd $ maximumBy (compare `on` fst) toCompare
            in breakGraph newR next'sccs

{-
addKeyfileIBoots :: Result -> Result
addKeyfileIBoots rIn =
  let sktbs = Set.fromDistinctAscList . map fst . filter (\(_k,v) -> v==SplitKeyTypeBoot) . Map.assocs . rKind $ rIn
      self = Set.map (\k -> (keyFile k,k)) sktbs
      new = Set.map (\(f,t) -> (keyFile f,t)) .
            Set.filter (\(f,_t) -> Set.member f sktbs) . rIBoot $ rIn
  in rIn { rIBoot = Set.unions [rIBoot rIn,new,self] }

keyFile :: MKey -> MKey
keyFile (FMName s) = FMName (s++"'Key")
-}

{-

reducing unneeded SOURCE annotations

2/AB:

A KeyTypeBoot
B TypeBoot

They are initially both tagged as TypeBoot.

AB/AB/A.hs:7:0:
    Warning: Unnecessary {-# SOURCE #-} in the import of module `AB.AB.B'
Ok, modules loaded: AB.AB, AB.AB.A, AB.AB.A, AB.AB.B, AB.AB.B.

./AB/AB/A.hs
module AB.AB.A (A(..), akeyba) where
import qualified AB.AB as AB (keyab, keyaa)
import qualified AB.AB.B as AB.B (bkeyab)
import {-# SOURCE #-} qualified AB.AB.B as AB (B)
       ^^^^^^^^^^^^^^

./AB/AB/A.hs-boot
module AB.AB.A (A, akeyba) where
import {-# SOURCE #-} qualified AB.AB.B as AB (B)

./AB/AB/B.hs
module AB.AB.B (B(..), bkeyab) where
import qualified AB.AB as AB (keyba, keybb)
import {-# SOURCE #-} qualified AB.AB.A as AB (A)
import {-# SOURCE #-} qualified AB.AB.A as AB.A (akeyba)

./AB/AB/B.hs-boot
module AB.AB.B (B) where

./AB/AB.hs
module AB.AB (keyab, keyaa, keyba, keybb, protoInfo, fileDescriptorProto) where
import Text.DescriptorProtos.FileDescriptorProto (FileDescriptorProto)
import Text.ProtocolBuffers.Reflections (ProtoInfo)
import qualified Text.ProtocolBuffers.WireMessage as P' (wireGet,getFromBS)
import {-# SOURCE #-} qualified AB.AB.A as AB (A)
import {-# SOURCE #-} qualified AB.AB.B as AB (B)

Need to express that A.hs-boot should SOURCE import B but that A proper does not need to import with SOURCE.

Why?  Everything aimed at a KeyTypeBoot can aim at its boot file, so
nothing aims at the main file, so it can import normal stuff!

Same thing applies to normal files for the SplitKeyTypeBoots.

Special case importPN and importPFN that KeyTypeBoot _normal_ files never use 

-}
