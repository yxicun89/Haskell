import System.Environment
import Control.Exception
import Data.List
import Debug.Trace

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
  | otherwise                   = checkSeqSub seq0
    where
      -- 定理1.2を用いて点の列seqがグラフ的であるかを再帰的に調べる
      checkSeqSub seq
        -- 負の数が出てくればグラフ的でないので、Falseを返す
        | last fc < 0        = False
        -- 0, ..., 0はグラフ的なので、Trueを返す
        | fq == 0            = True
        -- 定理1.2を適用
        | otherwise               = checkSeqSub ( (map (subtract 1) (take fq fh) ++ (drop fq fh)))
          where
            -- seqを大きい順にソートした結果→fc
            fc = (reverse.sort) seq
            -- fqはfcの先頭、fhはfcの2番目以降
            -- ついでにtraceShowで現在の数列を表示する
            (fq:fh) = traceShow fc fc

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
  catch 
    (putStrLn $ if y then "グラフ的である" else "グラフ的ではない" )
    (\ e -> return (e::SomeException) >> putStrLn "引数は整数で入力してください")
