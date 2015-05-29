import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Control.Applicative as A

-- (5 баллов)

-- 1. Определить класс MapLike типов, похожих на Map.
--    В нем должны быть функции empty, lookup, insert, delete, fromList с типами как в Data.Map.
--    Напишите реализацию по умолчанию для fromList.

-- 2. Определить instance MapLike для Data.Map, ListMap и ArrMap
--    Можно использовать любые стандартные функции.

class MapLike ml where
    empty :: ml k v
    lookup :: Ord k => k -> ml k v -> Maybe v
    insert :: Ord k => k -> v -> ml k v -> ml k v
    delete :: Ord k => k -> ml k v -> ml k v
    fromList :: Ord k => [(k, v)] -> ml k v    
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs) 

instance MapLike M.Map where
	empty = M.empty
	lookup = M.lookup
	insert = M.insert
	delete = M.delete      

newtype ListMap k v = ListMap [(k,v)]

instance MapLike ListMap where
   	empty = ListMap []
   	lookup k (ListMap xs) = L.lookup k xs   	
   	insert k v (ListMap xs) = ListMap $ [(k,v)] ++ L.filter (\(k',_)-> k /= k') xs 
   	delete k (ListMap xs) = ListMap $ fst $ L.partition (\(k',_)-> k /= k') xs
   	fromList = ListMap


newtype ArrMap k v = ArrMap (k -> Maybe v)

instance MapLike ArrMap where
   	empty = ArrMap (\_ -> Nothing)
   	lookup k (ArrMap fn) = fn k
   	insert k v (ArrMap fn) = ArrMap (\k' -> if k == k' then Just v else fn k) 
   	delete k (ArrMap fn) = ArrMap (\k' -> if k == k' then Nothing else fn k)


-- 3. Написать instace Functor для ListMap k и ArrMap k.

instance Functor (ListMap k) where
	fmap f (ListMap xs) = ListMap $ map (\(k,v)->(k,f v)) xs

instance Functor (ArrMap k) where
	fmap f (ArrMap ff) = ArrMap (\k -> fmap f $ ff k)    	
