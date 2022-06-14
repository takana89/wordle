# wordle

単純な対話プログラムの雛形

対話関数 `String -> String` を対話プログラムにするには、Prelude 関数 `interact :: (String -> String) -> IO ()` を使う。

`drive :: ([String] -> [String]) -> (String -> String)` をつくる。

```haskell
drive :: ([String] -> [String]) -> (String -> String)`
drive f = concat . f . lines
```

複数回の入力を処理して、複数回出力する関数は `[String] -> [String]`である。`responder :: String -> ([String] -> [String])` 関数で表現される。
`responder` の実態は、`MachineState` 上の状態遷移系である。

初期状態を生成する `initial :: String -> [String] -> MachineState` および `eval :: MachineState -> [MachineState]` の下請けである `isFinal :: MachineState -> Bool` と `step :: MachineState -> MachineState` を実装すればよい。
