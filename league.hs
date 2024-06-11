-- リーグ戦の日程を作成
-- 引数　チーム数
import System.Environment

-- nチームからなる円周上から対戦を選ぶための計算するための補助。
-- 計算の結果xが1からnに収まらないとき、すなわち
-- x=kn+r (r=1...n, k=...,-2, -1, 0, 1, 2, ...) に対して、
-- rを計算する。
mymod::Int->Int->Int
mymod x n =
  if x>0 then 1 + ((x-1) `mod` n)
         else n - ((-x) `mod` n)

-- xkチームのリーグ戦の第xs日目の第xw試合を作成。
-- 円周上にチーム1, 2, ..., xk-1を、中央にチームxkを配置し、
-- 第1試合から最終の1つ前の試合は円周上で
-- チームxsのxwだけ右とxwだけ左のチームを、
-- 最終試合は中央のチームとチームxsを対戦させる。
-- mymod については、この上に説明と定義がある。
match::Int->Int->Int->String
match xk xs xw
  -- xz, xjに対戦チーム番号を入れる
  = let (xz, xj) 
         = if (xw*2 == xk) 
             -- 第xs日の最終試合は中央のチーム(すなわちxk)対xs
             then (xs, xk)
             -- それ以外は円周上のチームが対戦
             else (mymod (xs+xw) (xk-1), mymod (xs-xw) (xk-1))
    -- 文字列 "xz-xj"を値として返す
    in (show xz) ++ "-" ++ (show xj)

-- nteamチームのリーグ戦の第day日目のスケジュールを作成
makeday::Int->Int->String
makeday nteam day
  = let 
    -- nteam2：チーム数が奇数の場合のダミーを加えたチーム数
        nteam2 = (nteam+(nteam `mod` 2))
    -- nmatch：1日の試合数
        nmatch = nteam `div` 2
    -- matchesに第1試合から第nmatch試合の組み合わせを求める。
        matches = map (match nteam2 day) [1..nmatch]
    -- 表示
    in "Day "++(show day)++"\n"++(unwords matches)++"\n"

-- メイン
main::IO ()
main = do
  -- 引数を読み込む
  args <- getArgs
  let nteam = read (head args)
  let nday = nteam - 1 + (nteam `mod` 2)
  putStr $ foldl (++) "" $ map (makeday nteam) [1..nday]
