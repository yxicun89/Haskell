import System.Environment

-- 二分木の定義
data Tree a = Nil | Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

-- 部分木の高さを求める
-- 必要ならば、2つの数の最大、最小を求める関数 max, minを使ってもよい。
height Nil = -1
height (Leaf _) = 0          -- 葉の高さは0
height (Node _ Nil Nil) = 0  -- 葉の高さは0
height (Node _ left right)
  = let
      ry_l = height left    -- 左部分木(left)の高さ
      ry_r = height right   -- 右部分木(right)の高さ
    in max ry_l ry_r + 1

-- 部分木の点(内点と葉)の総数を求める
numnodes Nil = 0
numnodes (Leaf _) = 1         -- 葉の点の個数は1
numnodes (Node _ left right)  -- 葉の点の個数は1
  = let
      ry_l = numnodes left   -- 左部分木(left)の点の総数
      ry_r = numnodes right  -- 右部分木(right)の点の総数
    in 1+ry_l+ry_r          -- 左右部分木の点の総数と自分自身(1個)の和

-- 部分木の葉の総数を求める
numleaves Nil = 0
numleaves (Leaf _) = 1         -- 葉の1つの葉からなる
numleaves (Node _ Nil Nil) = 1 -- 葉の1つの葉からなる
numleaves (Node _ left right)
  = let
      ry_l = numleaves left   -- 左部分木(left)の葉の数
      ry_r = numleaves right  -- 右部分木(right)の葉の数
       in ry_l + ry_r
-- メインルーチン
-- コマンドライン引数として与えられた二分木の高さを求める
main::IO ()
main = do
    args <- getArgs
    s <- readFile (args !! 0)
    let t = read s::Tree Int
    putStrLn $ "Height="++show (height t)
    putStrLn $ "Number of nodes="++show (numnodes t)
    putStrLn $ "Number of leaves="++show (numleaves t)
