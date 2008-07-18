-- | Derives an instance of @Monoid@. This derivation is limited to
-- data types with only one constructor; it uses the product
-- construction of monoids.
module Text.ProtocolBuffers.DeriveMergeable(makeMergeable,makeMergeableEnum) where

import Text.ProtocolBuffers.Mergeable
import Language.Haskell.TH.All

import Control.Monad(liftM2)
import Data.Generics
import Data.DeriveTH

data Foo
instance Mergeable Foo where

example = (,) "Mergeable" [d|
  instance Mergeable (Maybe Foo) where
    mergeEmpty = Nothing
    mergeAppend = mayMerge
 |]

--makeMergeable t = liftM2 (++) (derive makeMergeable1 t) (derive makeMergeable2 t)
makeMergeable t = derive makeMergeable1 t

makeMergeableEnum t = derive makeMergeable1Enum t
-- makeMergeableEnum t = liftM2 (++) (derive makeMergeable1Enum t) (derive makeMergeable2Enum t)

makeMergeable1 :: Derivation
makeMergeable1 = derivation mergeable1' "Mergeable"

mergeable1' dat | length (dataCtors dat) == 1
         = [instance_default "Mergeable" dat [funN "mergeEmpty" [empty],funN "mergeAppend" [append]]]
    where
        ctor = head $ dataCtors dat

        empty  = sclause [] $ lK (ctorName ctor) (replicate (ctorArity ctor) (l0 "mergeEmpty"))
        append = sclause [ctp ctor 'x',ctp ctor 'y'] $
                         lK (ctorName ctor) (zipWith (l2 "mergeAppend") (ctv ctor 'x') (ctv ctor 'y'))

mergeable1' dat = []
{-
makeMergeable2 :: Derivation
makeMergeable2 = derivation mergeable2' "Mergeable2"
--  instance Mergeable (Maybe a) where
mergeable2' dat = [InstanceD []
                             (AppT (ConT (mkName "Mergeable"))
                                         (AppT (ConT (mkName "Maybe"))
                                               (ConT (mkName foo))))
                             [(ValD (VarP (mkName "mergeEmpty"))
                                    (NormalB (ConE (mkName "Nothing"))) [])
                             ,(ValD (VarP (mkName "mergeAppend"))
                                    (NormalB (VarE (mkName "mayMerge"))) [])]]
  where foo = dataName dat
-}

makeMergeable1Enum :: Derivation
makeMergeable1Enum = derivation mergeable1Enum' "Mergeable1Enum"
--  instance Mergeable (Maybe a) where
mergeable1Enum' dat = [InstanceD []
                             (AppT (ConT (mkName "Mergeable"))
                                         (ConT (mkName foo)))
                             []]
  where foo = dataName dat
{-
makeMergeable2Enum :: Derivation
makeMergeable2Enum = derivation mergeable2Enum' "Mergeable2Enum"
--  instance Mergeable (Maybe a) where
mergeable2Enum' dat = [InstanceD []
                             (AppT (ConT (mkName "Mergeable"))
                                         (AppT (ConT (mkName "Maybe"))
                                               (ConT (mkName foo))))
                             [(ValD (VarP (mkName "mergeEmpty"))
                                    (NormalB (ConE (mkName "Nothing"))) [])
                             ,(ValD (VarP (mkName "mergeAppend"))
                                    (NormalB (VarE (mkName "mayMerge"))) [])]]
  where foo = dataName dat
-}
{-
--  instance Mergeable a => Mergeable (Maybe a) where
mergeable2' dat = [InstanceD (concat ([[(AppT (ConT (mkName "Mergeable"))
                                              (ConT (mkName foo)))]]))
                             (head [(AppT (ConT (mkName "Mergeable"))
                                          (AppT (ConT (mkName "Maybe"))
                                                (ConT (mkName foo))))])
                             [(ValD (VarP (mkName "mergeEmpty"))
                                    (NormalB (ConE (mkName "Nothing"))) [])
                             ,(ValD (VarP (mkName "mergeAppend"))
                                    (NormalB (VarE (mkName "mayMerge"))) [])]]
  where foo = dataName dat

-}