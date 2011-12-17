import Data.List
import System.Random

-- Setting
randomInitNo=1
comp=70

-- Data Type Definitions
data Cell = Cell {
	position::(Int,Int),
	east,west,south,north::Bool
	} deriving Eq

instance Show (Cell) where
  show (Cell pos e w s n) = 
    "<cell position=\"" ++ (show pos\\"()") ++ "\" "
    ++ "doors=\"" ++  "East:"  ++ (show e)
                  ++ ",West:"  ++ (show w)
                  ++ ",South:" ++ (show s)
                  ++ ",North:" ++ (show n) ++ "\" />"

i2b _i _x = 1 == _i `div` 2^_x `mod` 2

rndGen = mkStdGen randomInitNo

randList x = randomRs (0, x) rndGen

openCell (_x, _y) _size = Cell (_x, _y) (_x<_size) (_x>1) (_y<_size) (_y>1)
andCells (Cell p1 e1 w1 s1 n1) (Cell p2 e2 w2 s2 n2)
  |p1 == p2 = Cell p1 (e1 && e2) (w1 && w2) (s1 && s2) (n1 && n2)
  |otherwise = error "both of cells position must be same."

lockLevel (Cell _ e w s n)=
  foldl1 (+) (map(\a->if a then 0 else 1) [e,w,s,n])

getFieldLockNum _field = foldl1 (+) (map lockLevel _field)
getFieldComplex _field _size = _complex $ getFieldLockNum _field
  where
    _complex _lockNum = _lockNum * 100 `div` _maxWayNum
    _maxWayNum = _size^2*4 - _size*4

getNeighborPosition (Cell (x,y) e w s n) =
  snd (unzip (filter (\(tof,_)->tof)
      ([e,w,s,n] `zip`
        (map (\(x0,y0)->(x+x0,y+y0))
             ([1,-1,0,0] `zip` [0,0,1,-1])
        )
      )
  ))

getNeighborCells _cell _field
  = filter (\(Cell p _ _ _ _)->p `elem` getNeighborPosition _cell) _field
hasWay _field(Cell p _ _ _ _)
  = any (\c->p `elem` (getNeighborPosition c)) _field

