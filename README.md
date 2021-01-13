# x-inv-xeditor-dev
X/Inv/XEditorに操作変換の競合解決アルゴリズムを適用して  
2つのビュー間の更新内容の競合を解決しつつソースを更新できる$put$を提案します．

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
### 編集操作の表現
編集操作は次のように表現されます
```
type Path = [Int]

data Command a =
      Insert Path a
	| Delete Path a
	| EditLabel Path a
```
このプログラムでは`a`には`Val`が対応することを想定しています．


## 関数
### XEditor(app/EditorInf.hs)
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
type XMLState = (Content, Inv Val, Content)
editorMPutXML :: [(XMLState, Command Val)] -> Either (Err (Inv Val) Val) [XMLState]
```
XMLStateは(XMLソース，ファンクション，XMLビュー)をもつタプルです．
editorMPutXMLは，ビュー編集者の数を要素数とし，各々の編集者がXMLに対して行った編集操作`Command Val`
を持つ，`(State, Command Val)`のリストを入力とします．
出力は，各編集者のXMLStateを更新したものです．
Contentは[xml-1.3.14](https://hackage.haskell.org/package/xml-1.3.14)を使用しています．
