# x-inv-xeditor-dev
X/Inv/XEditorに操作変換の競合解決アルゴリズムを適用して  
2つのビュー間の更新内容の競合を解決しつつソースを更新できるmputを提案します．

X言語の論文はここから  
[A programmable editor for developing structured documents based on bidirectional transformations](https://doi.org/10.1007/s10990-008-9025-5)  
Zhenjiang Hu, Shin-Cheng Mu, and Masato Takeichi

もとのX/Inv/XEditorのソースコードは以下のサイトから  
[http://research.nii.ac.jp/~hu/project/bix.html]

操作変換の論文はここから  
[Verified Operational Transformation for Trees](https://doi.org/10.1007/978-3-319-43144-4_22)  
Sergey Sinchuk, Pavel Chuprikov, and Konstantin Solomatov


## 開発環境
* Debian 10 (buster) on WSL (仮想でないWindows 10でも動作確認済み)
* haskell stack 2.3.3
* GHC 8.8.4 (lts-16.16)

## 実行
このプログラムを実行する前に，stackの環境を用意してください．
このgitリポジトリをダウンロードし，そのディレクトリに移動します．そして，
`stack build`
をすると，必要なパッケージを取得します．GHCのインストールもここで行われるはずです．
完了したら，
`stack run`
を実行してください．プログラムがコンパイルされ，そのプログラムが実行されます．

## 使い方
### 構造化文書Valと編集操作の表現
`Val`とは簡易的な構造化文書(XML)とそれに施される編集操作をまとめて表現できる型です．
説明用の`Val`を用意します．`Val`は`Read`のインスタンスであり
`Val`用に用意されたシンタックスで導出できます．

```
src :: Val
src = read "{'a', {'b', \
\{'c', 'd':[]}:\
\{'e', 'f':[]}:\
\{'g', 'h':[]}:[]}:[]}"
```

ある一つの親ノードに対して，子ノードはリスト形式で表現されます．  
編集操作は次のように表現されます．

```
type Path = [Int]

data Command a =
      Insert Path a
	| Delete Path a
	| EditLabel Path a
	| Stay
```

このプログラムでは，`a`には`Val`が対応することを想定しています． 
`Path`は非負整数\[i1,i2,...in\]のリストであり，ルートのi1番目の子に入り，さらにi2番目の子に入る
といった動作をn階層分続けた先にある部分木を指します．  
`Command`のそれぞれの動作は次のようになります．

* `Insert Path a` : `Path`の位置に`a`を挿入する
* `Delete Path a` : `Path`の位置の部分木を削除する．`a`は操作変換の実装に合わせるために存在しており，  
`a`に何を入れても動作には影響しない．`Main.hs`では`'___'`をダミーとして使用
* `EditLabel Path a` : `Path`の位置のラベルを`a`に更新する 
* `Stay` : 何も編集しない

### 使用例
次のようなコマンドを用意します．

```
cmd1, cmd2, cmd3 :: Command Val
cmd1 = Insert [0,1] (read "{'x', 'y':[]}")
cmd2 = Delete [0,2] (read "'___'")
cmd3 = EditLabel [0,3] (read "'abc'")
```

ビューを生成します．

```
tar :: Val
tar = read ""

xsrc :: Content
xsrc = valToXML src

xtar :: Content
xtar = valToXML tar

v1 = extract $ editorGetXML (xsrc, transform, xtar)
v2 = extract $ editorGetXML (xsrc, transform, xtar)
```

`v1,v2`はソースの内容をそのまま抽出したビューを含む`State`となります．
次に，それぞれのビューに異なる更新を加え，mputを実行します．

```
s' = extract $ editorMPutXML [(v1, cmd1), (v2, cmd2)]
```

`s'`は，`v1,v2`の更新内容が競合解決されたソースになります．

### Main.hs
このファイルで定義されている`main`はテスト用の3つの関数を選択できるようになっています．

* `mputtest` : get -> mput -> get -> mput の順で実行する．get -> mputの時に必ず編集操作をはさむ
* `putgetput` : mput -> get -> mputがPUTGETPUTを満たしているかを確認する
* `getputget` : get -> mput -> getがGETPUTGETを満たしているかを確認する．

## 関数
### XEditor(app/EditorInf.hs)

#### editorGetXML
```
type XMLState = (Content, Inv Val, Content)
editorGetXML :: XMLState -> Either (Err (Inv Val) Val) XMLState
```
XMLStateは(XMLソース，ファンクション，XMLビュー)をもつタプルです．
Contentは[xml-1.3.14](https://hackage.haskell.org/package/xml-1.3.14)を使用しています．

#### editorMPut
```
type State = (Val, Inv Val, Val)

editorMPut :: [(State, Command Val)] -> Either (Err (Inv Val) Val) Val
```
Stateは(ソース, ファンクション, ビュー)をもつタプルです．
editorMPutは，ビュー編集者の数を要素数とし，各々の編集者がValに対して行った編集操作`Command Val`
を持つ，`(State, Command Val)`のリストを入力とします．
出力は，競合解決後の編集操作が適用されたソースです．

#### editorMPutXML
```
editorMPutXML :: [(XMLState, Command Val)] -> Either (Err (Inv Val) Val) [XMLState]
```
editorMPutXMLは，ビュー編集者の数を要素数とし，各々の編集者がXMLに対して行った編集操作`Command Val`
を持つ，`(State, Command Val)`のリストを入力とします．
出力は，各編集者のXMLStateを更新したものです．
