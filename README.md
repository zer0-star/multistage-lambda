## multistage-lambda
多段階let多相ラムダ計算のインタプリタ実装です

現時点では、多段階ではないlet多相ラムダ計算が実装してあります

入力文字列は、以下のパスに従って処理されます

- parser
- renamer
  - ここでnamedからde Bruijn indexを用いたnamelessな式に変換する
- type checker
- evaluator

`cabal run`で起動するインタプリタでは、入力を1行ずつ読み込み、パース結果・型チェック結果・評価結果を出力します。