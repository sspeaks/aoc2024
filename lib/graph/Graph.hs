module Graph where
import qualified Data.Map.Strict as M

class Point a where
    dist :: Num b => a -> a -> b

data Graph a where
    Graph :: Point a => M.Map a [a] -> Graph a
