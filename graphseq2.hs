import System.Environment
import Control.Exception
import Data.List

-- 数列がグラフ的であるかどうかを調べる
-- 入力:整数のリスト seq
-- 出力:seqがグラフ的ならばTrue、そうでなければFalse
checkSeq::[Int]->Bool
checkSeq seq0
  -- 点の個数以上の数が出てくればグラフ的でない
  | maximum seq0 >= length seq0 = False
  -- 数列の合計が奇数であればグラフ的でない
  | sum seq0 `mod` 2 == 1       = False
  -- そうでなければcheckSeqSub関数を用いて定理1.2を順次適用する
  | otherwise                = checkSeqSub seq0
    where
      -- 定理1.2を用いて点の列seqがグラフ的であるかを再帰的に調べる
      checkSeqSub seq
        -- 負の数が出てくればグラフ的でないので、Falseを返す
        | last sortedSeq < 0 = False
        -- 0, ..., 0はグラフ的なので、Trueを返す
        | k == 0             = True
        -- 定理1.2を適用
        | otherwise          = checkSeqSub ( (map (subtract 1) (take k ds)) ++ (drop k ds) )
          where
            -- seqを大きい順にソートした結果→sortedSeq
            sortedSeq = (reverse.sort) seq
            -- kはsortedSeqの先頭、dsはsortedSeqの2番目以降
            (k:ds) = sortedSeq

-- メインルーチン
main::IO ()
main = do
-- 引数を読んで
  args <- getArgs
-- 整数値のリストに変換
  let x = map( read::String->Int ) args
-- グラフ的かどうかを調べて結果を表示する
-- catchを用いるのは整数以外が入力されときに文句を言うため。
  let y = checkSeq x
      pr = length x
      pr3 = length $ filter (>=3) x
      pr4 = length $ filter (>=4) x
      py = (sum x) `div` 2
  catch 
    (putStrLn $ 
        if y then "グラフ的である\n" ++ 
                 -- 定理5.2などを用いて、点の数pr、辺の数py、次数3以上の点の数pr3、次数4以上の点の数pr4とから平面性をチェックする
                 if py > 3*pr - 6 then "平面的ではない"
                       else if (pr3 <= 5) && (pr4 <= 4) then "必ず平面的である"
                           else if py <= pr + 2 then "連結ならば必ず平面的である"
                               else "次数だけからは平面的であるかどうか判定できない"
             else "グラフ的ではない" )
    (\ e -> return (e::SomeException) >> putStrLn "引数は整数で入力してください")
