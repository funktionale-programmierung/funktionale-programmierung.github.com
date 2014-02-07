{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TransformListComp     #-}

import           Control.Monad
import           Data.Monoid
import           GHC.Exts (sortWith, groupWith, the)

import           Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set

import           Prelude        hiding (all, sum, (^))


-- see
--  - http://patternsinfp.wordpress.com/2012/01/19/comprehensions/
--  - http://wisnesky.net/wir11.pdf (Section 4 "Aggregation", "Kleisli form" of monad algebra)
class MonadPlus m => MonadAlgebra m a where
  reduce :: Monoid t => (a -> t) -> m a ->  t

instance MonadAlgebra [] a where
  reduce f = mconcat . map f

instance Ord a => MonadAlgebra Set a where
  reduce f = reduce f . Set.toList

-- Monad algebras
set :: (Ord a, MonadAlgebra m a) => m a -> Set a
list :: MonadAlgebra m a => m a -> [a]
some, all :: MonadAlgebra m Bool => m Bool -> Bool
sum :: (Num a, MonadAlgebra m a) => m a -> a
count :: MonadAlgebra m a => m a -> Int
exists :: MonadAlgebra m a => m a -> Bool

set  = reduce Set.singleton
list = reduce (:[])
some = getAny . reduce Any
all  = getAll . reduce All
sum  = getSum . reduce Sum
count = getSum . reduce (Sum . const 1)
exists = getAny . reduce (Any . const True)

(^) :: a -> (a -> b) -> b
(^) = flip ($)


-- Sample query comprehensions:
--
-- [ x | x <- [3, 2, 2]^list ]^sum
-- [ x | x <- [True, False, True]^list ]^all
-- [ [ z | z <- y ]^sum | (x,y) <- [(1,10), (2,40), (3,30)]^set, then group by (odd x) using grpWith ]^set
-- []^exists
-- [ x | x <- [3, 1, 2, 1]^list, then sortWith by x ]^list



{- TPC-H Q4
   Counts the number of orders ordered in a given quarter of a given year in
   which at least one lineitem was received by the customer later than its
   committed date.

    SELECT o_orderpriority, COUNT(*) as order_count
    FROM   orders
    WHERE  o_orderdate >= '1995-01-01'
    AND    o_orderdate <  '1995-04-01'
    AND    EXISTS (SELECT *
                   FROM   lineitem
                   WHERE  l_orderkey = o_orderkey
                   AND    l_commitdate < l_receiptdate)
    GROUP BY o_orderpriority
    ORDER BY o_orderpriority;
-}

q4 :: [(Int,Int)]
q4 =
  [ (the o_orderpriority, count o_orderkey) |
    (o_orderkey,o_orderdate,o_orderpriority) <- orders,
    o_orderdate >= "1995-01-01",
    o_orderdate <  "1995-04-01",
    [ 1 | (l_orderkey,l_commitdate,l_receiptdate) <- lineitem,
          l_orderkey == o_orderkey,
          l_commitdate < l_receiptdate
    ]^exists,
    then group by o_orderpriority using groupWith,
    then sortWith by o_orderpriority
  ]

main :: IO ()
main = do
  print q4

-- An excerpt of TPC-H data

orders :: [(Int,String,Int)]
orders = [
  (1, "1995-01-10", 3),
  (2, "1995-01-12", 1),
  (3, "1995-02-19", 5),
  (4, "1995-02-24", 5),
  (5, "1995-02-25", 2),
  (6, "1995-03-30", 2),
  (7, "1995-04-04", 4)]

lineitem :: [(Int,String,String)]
lineitem = [
  (1, "1995-01-12", "1995-01-13"),
  (1, "1995-01-12", "1995-01-13"),
  (1, "1995-01-13", "1995-01-13"),
  (2, "1995-01-14", "1995-01-12"),
  (2, "1995-01-14", "1995-01-13"),
  (3, "1995-02-24", "1995-02-27"),
  (4, "1995-02-26", "1995-02-27"),
  (4, "1995-02-26", "1995-02-27"),
  (5, "1995-02-27", "1995-02-27"),
  (5, "1995-02-28", "1995-03-01"),
  (6, "1995-04-01", "1995-03-30"),
  (7, "1995-04-06", "1995-04-08")]



