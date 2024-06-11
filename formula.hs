import System.Environment

-- 二分木の定義
data Tree a = Nil | Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

-- 二分木の読み込み
readTree::String->Tree String
readTree s = read $ addquote $ addspace s
  where
    addspace "" = ""
    addspace (x:xs) = if( x `elem` "()" ) 
                        then (' ':x:' ':(addspace xs)) 
                        else x:(addspace xs)
    addquote src = unwords ( map f ( words src ) )
      where
        f x = if( x `elem` ["Node", "Leaf", "Nil", "(", ")"] 
               || (head x)=='"' && (last x)=='"' ) then x else ('"':x++"\"")

-- 前置記法を求める
getPrefix Nil = ""
-- 葉の場合、変数または定数そのまま返す
getPrefix (Leaf a) = a
getPrefix (Node a Nil Nil) = a
-- 内点の場合　opは演算記号、leftは左部分木、rightは右部分木
getPrefix (Node op left right)
  = let
      zq = getPrefix left   -- 左部分木の式
      zh = getPrefix right  -- 右部分木の式
    in op ++ " "++ zq ++ " " ++ zh

-- 中置記法を求める
getInfix Nil = ""
-- 葉の場合、変数または定数そのまま返す
getInfix (Leaf a) = a
getInfix (Node a Nil Nil) = a
-- 内点の場合　opは演算記号、leftは左部分木、rightは右部分木
getInfix (Node op left right)
  = let
      zq = getInfix left   -- 左部分木の式
      zh = getInfix right  -- 右部分木の式
    in "(" ++ zq ++ op ++ zh ++ ")"

-- 後置記法を求める
getPostfix Nil = ""
-- 葉の場合、変数または定数そのまま返す
getPostfix (Leaf a) = a
getPostfix (Node a Nil Nil) = a
-- 内点の場合　opは演算記号、leftは左部分木、rightは右部分木
getPostfix (Node op left right)
  = let
      zq = getPostfix left   -- 左部分木の式
      zh = getPostfix right  -- 右部分木の式
      in zq ++ " " ++ zh ++ " " ++ op
-- メインルーチン
-- コマンドライン引数として与えられた木の式を求める
main::IO ()
main = do
    args <- getArgs
    let fname = args !! 0
    s <- readFile fname
    let t = readTree s
    putStrLn $ "Prefix : "++(getPrefix t)
    putStrLn $ "Infix  : "++(getInfix t)
    putStrLn $ "Postfix: "++(getPostfix t)

