module TimeRanges where

-- import Data.List hiding (union)
import SimpleTime


---- Continuous Time Range ----

data Begin
    = NegEternity
    | After Time
    deriving (Eq, Ord) -- Ord lets us use min/max

data End
    = Before Time
    | Eternity
    deriving (Eq, Ord) -- Ord lets us use min/max

data CTR
    = NoTime
    | CTR Begin End
    deriving (Eq, Ord) -- Ord to aid DTR normalization.

instance Show CTR where
    show NoTime                                   = "NoTime"
    show (CTR NegEternity Eternity)               = "AllTime"
    show (CTR (After time) Eternity)              = ">=" ++ show time
    show (CTR NegEternity (Before time))          = "<"  ++ show time
    show (CTR (After beginTime) (Before endTime)) = show beginTime ++ "-" ++ show endTime

instance Read CTR where
    -- Read is a little less straightforward because it uses parsers, which
    -- we'll learn about later. Just replace the undefineds below with what
    -- you want to return and everything will work.
    readsPrec _ "NoTime"          = [(NoTime, "")]
    readsPrec _ "AllTime"         = [(CTR NegEternity Eternity, "")]
    readsPrec _ ('>':'=':timeStr) = [(CTR (After $ read timeStr) Eternity, "")]
    readsPrec _ ('<':timeStr)     = [(CTR NegEternity (Before $ read timeStr), "")]
    readsPrec _ str =
        -- "2013-07-31T10:34:48-2016-12-16T19:11:45"
        [(CTR (After . read $ take 19 str) (Before . read $ drop 20 str), "")]


-- All functions that return a changed CTR should santize it.
sanitizeCTR :: CTR -> CTR
sanitizeCTR ctr@(CTR (After beginTime) (Before endTime))
    | endTime <= beginTime = NoTime
    | otherwise            = ctr
sanitizeCTR ctr = ctr

intersectCTRCTR :: CTR -> CTR -> CTR
intersectCTRCTR NoTime _  = NoTime
intersectCTRCTR _ NoTime  = NoTime
intersectCTRCTR (CTR begin1 end1) (CTR begin2 end2) =
    sanitizeCTR $ CTR (max begin1 begin2) (min end1 end2)


---- Discontinuous Time Range ----

-- The CTRs that make up a DTR may overlap
-- or touch. Do not worry about simplifying
-- the DTR. That is handled just before
-- display by normalizeDTR.

type DTR = [CTR]

allTimeDTR :: DTR
allTimeDTR = [CTR NegEternity Eternity]

noTimeDTR :: DTR
noTimeDTR = []

intersectDTRCTR :: DTR -> CTR -> DTR
intersectDTRCTR = flip (map . intersectCTRCTR)
--    undefined

-- CTR minus a CTR must return a DTR because the second
-- could cut a hole in the middle of the first.
differenceCTRCTR :: CTR -> CTR -> DTR
differenceCTRCTR NoTime _      = noTimeDTR
differenceCTRCTR ctr1   NoTime = [ctr1]
differenceCTRCTR (CTR begin1 end1) (CTR begin2 end2)
    | begin2 <= begin1 =
        case end2 of
            Before end2time -> [sanitizeCTR $ CTR (max begin1 (After end2time)) end1]
            Eternity        -> noTimeDTR
    | end2 >= end1 =
        case begin2 of
            NegEternity      -> noTimeDTR --Assuming that begin1 must also be NegEternity
            After begin2time -> [sanitizeCTR $ CTR begin1 (Before begin2time)]
    -- If you reach here, you know begin1 < begin2 and end2 < end1
    | otherwise =
        let (After begin2time, Before end2time) = (begin2, end2) in
        map sanitizeCTR $ [(CTR begin1 (Before begin2time)), (CTR (After end2time) end1)]

-- DTR minus a CTR
differenceDTRCTR :: DTR -> CTR -> DTR
differenceDTRCTR dtr ctr = concatMap  ((flip differenceCTRCTR) ctr) dtr



---- Discontinues Time Range Helpers ----

intersection :: DTR -> DTR -> DTR
intersection dtr1 dtr2 = concatMap (intersectDTRCTR dtr1) dtr2


union :: DTR -> DTR -> DTR
union = (++)

difference :: DTR -> DTR -> DTR
difference = foldr (flip differenceDTRCTR)
-- differenceDTRCTR
-- cant use concatmap cuz u need to alter the list as u go


---- Discontinues Time Range Queries ----

intersectAll :: [DTR] -> DTR
intersectAll = foldr intersection allTimeDTR
  -- Use foldr.

unionAll :: [DTR] -> DTR
unionAll = concat

-- Take the first and subtract each of the remaining.
differenceAll :: [DTR] -> DTR
differenceAll []           = noTimeDTR
differenceAll (first:rest) = foldr (flip difference) first rest

-- Use foldr.


---- Boolean Helpers ----

isEmpty :: DTR -> Bool
isEmpty = null . filter ((/=) NoTime)

-- Hint: areDisjoint and isubset are simpler than areEqual.
-- Use what you've already defined.

-- "Disjoint" means no overlap.
areDisjoint :: DTR -> DTR -> Bool
areDisjoint dtr1 dtr2 = isEmpty $ intersection dtr1 dtr2

isSubset :: DTR -> DTR -> Bool
isSubset dtr1 dtr2 = isEmpty $ difference dtr1 dtr2

areEqual :: DTR -> DTR -> Bool
areEqual dtr1 dtr2 =
    dtr1 `isSubset` dtr2 &&
    dtr2 `isSubset` dtr1

---- Boolean Queries ----

areAllDisjoint :: [DTR] -> Bool
areAllDisjoint []         = True
areAllDisjoint (dtr:dtrs) = (dtr `areDisjoint` (unionAll dtrs)) && (areAllDisjoint dtrs)

-- Hint: this function is recursive.

areAllEqual :: [DTR] -> Bool
areAllEqual []         = True
areAllEqual (dtr:dtrs) = False `notElem` map (areEqual dtr) dtrs
