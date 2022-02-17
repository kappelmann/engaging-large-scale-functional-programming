module Exercise09_Efficient where

import Types_Efficient

import Data.Function (on)
import qualified Data.IntSet as IS
import qualified Data.IntMap.Lazy as IM
import Control.Arrow (second)
import Data.List (sortBy, sortOn, groupBy)

-- FTrie
-- this is a path-compressed version of feature vector indexing as described in https://link.springer.com/chapter/10.1007/978-3-642-36675-8_3
-- we implement perfect subsumption checking by using all literals as features;
-- a Node stores whether a literal does not occur (left) or does occur (right) in a set of clause keys
data FTrie = Leaf IS.IntSet | Node FTrie Name IS.IntSet FTrie
  deriving (Eq, Show)

emptyFTrie :: FTrie
emptyFTrie = Leaf IS.empty

buildFTrie :: KeyConjForm -> FTrie
buildFTrie = run [] . group . sortOn snd . map (second IS.toAscList) . IM.toList
  where
    group = groupBy (groupHead `on` snd)
    groupHead _ [] = True
    groupHead [] _ = False
    groupHead (k1:_) (k2:_) = k1 == k2
    run :: [Key] -> [[(Key, [Literal])]] -> FTrie
    run lks [] = Leaf $ IS.fromList lks
    -- note: as we call run with groupBy, ther will always be at least one element in the list
    run lks gkcs@(gkc@((_, []):_):gkct) = run nLks gkct
      where nLks = lks ++ map fst gkc
    run lks gkcs@(gkc@((_, l:_):_):gkct) = Node left l (IS.fromList ks) right
      where
        ks = lks ++ concatMap (map fst) gkcs 
        left = run lks gkct
        right = run [] $ group $ map (second tail) gkc
    run _ _ = error "unreachable code"

insertFTrie :: Key -> Clause -> FTrie -> FTrie
insertFTrie k c fTrie = run fTrie $ IS.toAscList c
  where
    run (Leaf ks) [] = Leaf $ IS.insert k ks
    run (Node left l ks right) [] = Node (run left []) l (IS.insert k ks) right
    run t@(Leaf ks) (l:ls) = Node t l (IS.insert k ks) (run emptyFTrie ls)
    run t@(Node left l ks right) ls@(l':lt)
      | l < l' = Node (run left ls) l nks right
      | l == l' = Node left l nks (run right lt)
      | otherwise = Node t l' nks (run emptyFTrie lt)
      where nks = IS.insert k ks

fTrieSubsumes :: FTrie -> Clause -> Bool
fTrieSubsumes fTrie = find fTrie . IS.toAscList
  where
    find :: FTrie -> [Literal] -> Bool
    find (Leaf ks) _ = not $ IS.null ks
    find (Node left _ _ _) [] = find left []
    find t@(Node left l _ right) ls@(l':lt)
      | l < l' = find left ls
      | l == l' = find left lt || find right lt
      | otherwise = find t lt

filterSubsumedFTrie :: Clause -> FTrie -> (IS.IntSet, FTrie)
filterSubsumedFTrie c fTrie = filterAux fTrie $ IS.toAscList c
  where
    filterAux (Leaf ks) [] = (ks, emptyFTrie)
    filterAux (Leaf ks) _ = (IS.empty, Leaf ks)
    filterAux (Node _ _ ks _) [] = (ks, emptyFTrie)
    filterAux ftrie@(Node left l ks right) ls@(l':lt)
      | l < l' = let (ksl, fTrieL) = filterAux left ls
                     (ksr, fTrieR) = filterAux right ls in
        (ksl `IS.union` ksr, compress fTrieL fTrieR l $ ks IS.\\ (ksl `IS.union` ksr))
      | l == l' = let (ksr, fTrieR) = filterAux right lt in
        (ksr, compress left fTrieR l $ ks IS.\\ ksr)
      | otherwise = (IS.empty, ftrie)
    compress left@(Leaf ksl) right@(Leaf ksr) l ks
      | IS.null ksl && IS.null ksr = Leaf ks
      | otherwise = Node left l ks right
    compress left right l ks = Node left l ks right

filterSubsumedDataKeyConjFormFTrie :: Clause -> KeyDataConjForm -> FTrie -> (KeyDataConjForm, FTrie)
filterSubsumedDataKeyConjFormFTrie c kcs fTrie =
  let (ks, fTrie') = c `filterSubsumedFTrie` fTrie
      kcs' = kcs `IM.withoutKeys` ks in
  (kcs', fTrie')

-- util functions

instance Show Polarity where
  show Pos = ""
  show Neg = "~"

instance Show ELiteral where
  show (Literal p a) = show p ++ a

lPos :: Name -> Literal
lPos = id

lNeg :: Name -> Literal
lNeg = negate

lIsPos :: Literal -> Bool
lIsPos = (>0)

lIsNeg :: Literal -> Bool
lIsNeg = (<0)

lNegate :: Literal -> Literal
lNegate = negate

lName :: Literal -> Name
lName = abs

-- evaluations

evalLiteral :: Valuation -> Literal -> Bool
evalLiteral val l
  | lIsPos l = l `IS.member` val
  | otherwise = lNegate l `IS.notMember` val

evalClause :: Valuation -> Clause -> Bool
evalClause val = IS.foldr ((||) . evalLiteral val) False

eval :: Valuation -> ConjForm -> Bool
eval = all . evalClause

clauseIsTauto :: Clause -> Bool
clauseIsTauto c = IS.foldr ((||) . (`IS.member` c) . lNegate) False c

-- basic resolution functions

complements :: Literal -> Literal -> Bool
complements n n' = n == -n'

resolve :: Name -> Clause -> Clause -> Clause
resolve n cp cn = IS.delete (lPos n) cp `IS.union` IS.delete (lNeg n) cn

-- proof checking

proofCheck :: EConjForm -> Proof -> Bool
proofCheck cs (Model' val) = eval val (toConjForm cs)
proofCheck initECs (Refutation' initRs) = run (IM.size initCs) initCs initRs
  where 
    initCs = toConjForm initECs
    run _ cs [] = IS.empty `elem` cs
    run i cs (Resolve' n k1 k2:rs) = run (i+1) (IM.insert i c cs) rs
      where c = resolve n (cs IM.! k1) (cs IM.! k2)

-- homework starts here

-- selection strategies

selClause :: SelClauseStrategy 
selClause q
  | IM.null q = Nothing 
  | otherwise = let (k, kdcs) = IM.findMin q -- pick the smallest clause
                    (kdc, kdcs') = IM.deleteFindMin kdcs in
    Just (kdc, IM.update (const $ queueMaybeChild kdcs') k q)

queueMaybeChild :: KeyDataConjForm -> Maybe KeyDataConjForm
queueMaybeChild kdcs
  | IM.null kdcs = Nothing
  | otherwise  = Just kdcs

selLiterals :: SelLiteralsStrategy
selLiterals ((mi,_),_)
  | lIsNeg mi = IS.singleton mi
  | otherwise = IS.empty

resolvants :: SelLiteralsStrategy -> KeyDataClause -> KeyDataClause -> [(Resolve, Clause)]
resolvants selLiterals kdc1@(_,dc1) kdc2@(_,dc2) =
  let sel1 = selLiterals dc1
      sel2 = selLiterals dc2 in
  produce kdc1 sel1 kdc2 sel2 ++ produce kdc2 sel2 kdc1 sel1
  where
    produce (kp,((_,lmp),cp)) selp (kn,((_,lmn),cn)) seln
      | lIsNeg lmp || not (IS.null selp) = []
      | IS.null seln = [rc | lmp `complements` lmn]
      | otherwise = [rc | lNegate lmp `IS.member` seln]
      where rc = let n = lName lmp in (Resolve' n kp kn, resolve n cp cn)

isTrivial :: Clause -> Bool
isTrivial = clauseIsTauto

preProcess :: ConjForm -> KeyConjForm
preProcess = IM.filter (not . isTrivial) -- remove trivial clauses

resolutionParam :: SelClauseStrategy -> SelLiteralsStrategy -> ConjForm -> (Proof, KeyConjForm)
resolutionParam selClause selLiterals csInit = run [] (IM.size csInit) qInit IM.empty emptyFTrie
  where
    preProcessed = preProcess csInit
    qInit = keyConjFormToQueue preProcessed
    run :: [Resolve] -> Key -> Queue -> KeyDataConjForm -> FTrie -> (Proof, KeyConjForm)
    run rs k uqkdcs pkdcs pFTrie = case selClause uqkdcs of -- pick next clause
      -- everything is saturated -> extract the model
      Nothing -> let pkcs = IM.map snd pkdcs in (Model' $ extractModel pkcs, pkcs)
      Just (ukdc@(uk,udc@(_,uc)), uqkdcs') -> 
        -- found the empty clause -> return the resolution proof
        if IS.null uc then (Refutation' rs, IM.map snd pkdcs `IM.union` queueToKeyConjForm uqkdcs')
        else if pFTrie `fTrieSubsumes` uc then run rs k uqkdcs' pkdcs pFTrie -- clause subsumed -> continue
        else 
          let (pkdcs', pFTrie') = filterSubsumedDataKeyConjFormFTrie uc pkdcs pFTrie -- remove clauses subsumed by uc
              (nrs, ncs) = unzip
                 [(r, c) | (r, c) <- concatMap (resolvants selLiterals ukdc) $ IM.toList pkdcs', -- get all candidates
                           not $ isTrivial c] -- skip trivial clauses
              nuqkdcs = zip [k..] ncs `addToQueue` uqkdcs' -- add new clauses to queue
              pkdcs'' = IM.insert uk udc pkdcs' -- add clause to processed set
              pFTrie'' = insertFTrie uk uc pFTrie' in -- add clause to FTrie
          run (rs ++ nrs) (k + length ncs) nuqkdcs pkdcs'' pFTrie'' -- continue
    addToQueue :: [(Key, Clause)] -> Queue -> Queue
    addToQueue kcs q = foldr (\(k,c) -> IM.insertWith IM.union (IS.size c) $ IM.singleton k (lData c, c)) q kcs

resolution :: EConjForm -> (Proof, KeyConjForm)
resolution = resolutionParam selClause selLiterals . toConjForm

lCompare :: Literal -> Literal -> Ordering
lCompare l1 l2
  | abs l1 == abs l2 = compare l2 l1
  | otherwise = (compare `on` abs) l1 l2

-- as described on https://lara.epfl.ch/w/_media/sav08/gbtalk.pdf
extractModel :: ConjForm -> Valuation
extractModel cs = run IS.empty $ sortOn snd $ map (second $ sortBy (flip lCompare) . IS.toList) $ IM.toList cs
  where
    run :: Valuation -> [(Key, [Literal])] -> Valuation
    run val [] = val
    run val ((k,c):kcs)
      | evalClause val (cs IM.! k) = run val kcs
      | otherwise = run (IS.insert (head c) val) kcs

