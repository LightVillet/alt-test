module EVRComparison (compareEVR) where
-- Based on rpmvercmp algorithm
-- See https://fedoraproject.org/wiki/Archive:Tools/RPM/VersionComparison

-- So far I don't want to implement my own data type for version just for using pretty LE, EQ and GT
-- Because I will lose all benefits of strings or need to implement it by my own

-- TODO: add tests

import Data.Char
import Text.Read (readMaybe)

type Epoch = Integer
type Label = String
type EVR = (Epoch, Label, Label)

split :: Label -> [String]
split [] = []
split a@(x:_)
    | isAlpha x     = takeWhile isAlpha a : split (dropWhile isAlpha a)
    | isNumber x    = takeWhile isNumber a : split (dropWhile isNumber a)
    | otherwise     = split $ dropWhile (not . isAlphaNum) a

compareElem :: String -> String -> Ordering
compareElem s1 s2 =
    case (x1, x2) of
        (Just n1, Just n2)  -> compare n1 n2
        (Just _, Nothing)   -> GT
        (Nothing, Just _)   -> LT
        (Nothing, Nothing)  -> compare s1 s2
    where   x1 = readMaybe s1 :: Maybe Int
            x2 = readMaybe s2 :: Maybe Int

compareSplitted :: [String] -> [String] -> Ordering
compareSplitted [] [] = EQ
compareSplitted [] (_:_) = LT
compareSplitted (_:_) [] = GT
compareSplitted (a:as) (b:bs) = 
    case cmp of
        EQ  -> compareSplitted as bs
        _   -> cmp
    where cmp = compareElem a b

compareLables :: Label -> Label -> Ordering
compareLables l1 l2 = compareSplitted (split l1) (split l2)

compareEVR :: EVR -> EVR -> Ordering
compareEVR (e1, v1, r1) (e2, v2, r2)
    | e1 /= e2      = compare e1 e2
    | cmpV /= EQ    = cmpV
    | otherwise     = compareLables r1 r2
    where cmpV = compareLables v1 v2