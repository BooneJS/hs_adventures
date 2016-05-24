-- A List implemented as a Zipper
-- https://en.wikipedia.org/wiki/Zipper_(data_structure)

-- Node Value LeftList RightList
data Node a = MkNode a [a] [a]
  deriving (Eq, Show)

isTail :: Node a -> Bool
isTail (MkNode _ _ []) = True
isTail _ = False

isHead :: Node a -> Bool
isHead (MkNode _ [] _) = True
isHead _ = False

goRight :: Node a -> Node a
goRight nnn@(MkNode _ _ []) = nnn
goRight (MkNode n l (r:rs)) = MkNode r (n : l) rs

goLeft :: Node a -> Node a
goLeft nnn@(MkNode _ [] _) = nnn
goLeft (MkNode n (l:ls) r) = MkNode l ls (n : r)

insertRight :: a -> Node a -> Node a
insertRight n (MkNode nn l r) = MkNode n l (nn : r)

main :: IO ()
main = do
  let a' = MkNode 5 [] []
  print a'
  print (isHead a')
  print (isTail a')
  let b' = insertRight 2 a'
  print b'
  let c' = insertRight 3 b'
  print c'
  let d' = goRight c'
  print d'
  let e' = goRight d'
  print e'
  let f' = goLeft (goLeft e')
  print f'

  let aa' = MkNode "Blah" [] []
  print aa'
