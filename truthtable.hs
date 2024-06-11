import System.Environment
import Data.List
import Data.Maybe

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

-- 演算子の定義
opTRUE   = -1::Int
opFALSE  = -2::Int
opAND    = -3::Int
opOR     = -4::Int
opNOT    = -5::Int
opXOR    = -6::Int
opEQUIV  = -7::Int
opIMP    = -8::Int

getVars::Tree String->[String]
getVars=(nub.sort.getVarsSub)
getVarsSub Nil = []
getVarsSub (Leaf s) = if (elem s ["T", "F", "0", "1"] ) then [] else [s]
getVarsSub (Node s Nil Nil) = if (elem s ["T", "F", "0", "1"] ) then [] else [s]
getVarsSub (Node _ tl tr) = (getVarsSub tl)++(getVarsSub tr)

-- 与えられた木trをノードが演算子を表すTntであるような木に変換する。
convTree::[String]->Tree String-> Tree Int
convTree vars tr = convTreeSub tr
  where
    convTreeSub Nil=Nil
    convTreeSub (Leaf s) = Leaf (leaf2var s)
    convTreeSub (Node s Nil Nil) = Leaf (leaf2var s)
    convTreeSub (Node s tl tr) = Node (node2op s) (convTreeSub tl) (convTreeSub tr)
    leaf2var x | elem x ["1", "T"] = opTRUE
               | elem x ["0", "F"] = opFALSE
               | otherwise = fromJust (elemIndex x vars)::Int
    node2op  x | elem x ["&", "&&", "*"] = opAND
               | elem x ["|", "||", "+"] = opOR
               | elem x ["!", "~"] = opNOT
               | elem x ["^", "!="] = opXOR
               | elem x ["=", "=="] = opEQUIV
               | elem x [">", "->"] = opIMP

-- 式の評価
-- 入力: 式中の変数の値のリスト、式を表す木
-- 出力: 式の値
eval::[Bool]->Tree Int->Bool
eval table tree = evalTreeSub tree
  where
    evalTreeSub Nil = True
    -- 葉の場合、点のラベルxが「真(opTRUE)」であるか、xの表す変数が真であれば真
    evalTreeSub (Leaf s) = (s==opTRUE) || (s>=0 && table !! s)
    -- 内点の場合、左右の部分木を評価して、それらから点のラベルsに従って演算する
    evalTreeSub (Node s left right)
      | s==opAND    = tt_l && tt_r      -- 論理積 (tt_l ∧ tt_r)
      | s==opOR     = tt_l || tt_r  -- 論理和 (tt_l ∨ tt_r)
      | s==opNOT    = not tt_r      -- 否定 (￢tt_r)
      | s==opXOR    = tt_l /= tt_r  -- 排他的論理和 (tt_l XOR tt_r)
      | s==opEQUIV  = tt_l == tt_r  -- 等価 (tt_l ≡ tt_r)
      | s==opIMP    = (not tt_l) || tt_r        -- 含意 (tt_l → tt_r)
      where
        tt_l = evalTreeSub left   -- 左部分木の値
        tt_r = evalTreeSub right  -- 右部分木の値


-- 真理値表を作る。
-- 入力: 計算の木、出力: 真理値表
truthTable::Tree String->String
truthTable tree = title ++ "\n" ++ nextRow (replicate (length vars) False)
  where
    vars = getVars tree
    title = foldr (\x y->x++"\t"++y) "" vars
    formula = convTree vars tree
      -- 真理値表出力の本体
      -- 各変数の値(boolean)からなるリストbitsを引数とし、論理式を評価する。
      -- さらに、T=1, F=0とみなした2進数に1を加えることで次の変数の値の組を求め、計算を繰り返す。
      -- 例えば bits = [*,...,*, False, True,...,True]ならば次の組は [*,...,*, True, False,...,False]
      -- ただし、bits = [True, True, ..., True] ならば、これでおしまいなので繰り返すことはしない。
    nextRow bits
      | null yy   = r
      | otherwise = r ++ nextRow (reverse (True:tail yy) ++ replicate (length xx) False)
      where
        r = (concat (map (\b -> if b then "T\t" else "F\t") bits)) 
             ++ (if eval bits formula then "T" else "F")++"\n"
        (xx,yy)=break (not) (reverse bits)

-- メインルーチン
main::IO ()
main = do
  args <- getArgs
  z <- if (null args) || ((head args)=="-")
    then getLine
    else readFile(head args)
  putStr $ truthTable (readTree z)
