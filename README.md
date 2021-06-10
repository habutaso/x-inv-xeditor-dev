# x-inv-xeditor-dev
X/Inv/XEditorに操作変換の競合解決アルゴリズムを適用して  
2つのビュー間の更新内容の競合を解決しつつソースを更新できるmputを提案します．  
We propose mput, which applies a conflict resolution algorithm for operational transformations to X/Inv/XEditor.

X言語の論文はここから  
The paper on X langauge we refer to is the following  
[A programmable editor for developing structured documents based on bidirectional transformations](https://doi.org/10.1007/s10990-008-9025-5)  
Zhenjiang Hu, Shin-Cheng Mu, and Masato Takeichi  
High. Order Symb. Comput. 21(1-2),  89-118 (2008)

もとのX/Inv/XEditorのソースコードは以下のサイトから  
Source code for X/Inv/XEditor.  
[http://research.nii.ac.jp/~hu/project/bix.html]

操作変換の論文はここから  
The paper on operational transformation we refer to is the following  
[Verified Operational Transformation for Trees](https://doi.org/10.1007/978-3-319-43144-4_22)  
Sergey Sinchuk, Pavel Chuprikov, and Konstantin Solomatov  
In: International Conference on Interactive Theorem Proving. pp. 358-373. Springer (2016)


## 開発環境 (Developing environment)
* Debian 10 (buster) on WSL (仮想でないWindows 10でも動作確認済み)(Also confirmed to work in non-virtual Windows 10 environment)
* haskell stack 2.3.3
* GHC 8.8.4 (lts-16.16)

## 実行 (How to run the program)
このプログラムを実行する前に，stackの環境を用意してください．  
Before running this program, please prepare stack.  
このgitリポジトリをダウンロードし，そのディレクトリに移動します．そして， 
Download this git repository, and go to the directory. The following command line  
`stack build`  
をすると，必要なパッケージを取得します．GHCのインストールもここで行われるはずです．
完了したら，  
will retrieve the necessary packages. GHC should be installed in this step.  
Then run the following command.  
`stack run`
を実行してください．プログラムがコンパイルされ，そのプログラムが実行されます．  
It should compile the program and run.  

## 使い方 (Usage)
### 構造化文書Valと編集操作の表現 (Structured document Val and representation of editing operation)
`Val`とは簡易的な構造化文書(XML)とそれに施される編集操作をまとめて表現できる型です．
説明用の`Val`を用意します．`Val`は`Read`のインスタンスであり
`Val`用に用意されたシンタックスで導出できます．  
`Val`is a type that can collectively represent simple structured document
(XML) and the editing operations applied to it．
Here we prepare `Val` for explanatory purpose. `Val` is an instance of `Read`,
and can be derived using the syntax prepared for `Val`.


```
src :: Val
src = read "{'a', {'b', \
\{'c', 'd':[]}:\
\{'e', 'f':[]}:\
\{'g', 'h':[]}:[]}:[]}"
```

ある一つの親ノードに対して，子ノードはリスト形式で表現されます．  
編集操作は次のように表現されます．  
For a given parent node, its children nodes are represented in list format.  
Editing operations are represented as follows.

```
type Path = [Int]

data Command a =
      Insert Path a
	| Delete Path a
	| EditLabel Path a
	| Stay
```

このプログラムでは，`a`には`Val`が対応することを想定しています．  
This program assumes that `a` corresponds to `Val`.  
`Path`は非負整数\[i1,i2,...in\]のリストであり，ルートのi1番目の子に入り，さらにi2番目の子に入る
といった動作をn階層分続けた先にある部分木を指します．  
`Path` is a list of non-negative integers \[i1,i2,...in\]，representing the navigation
to proceed to the i1-th child of the root, then to its i2-th child, and so on,
to represent the subtree reached after repeating this step n times.  
`Command`のそれぞれの動作は次のようになります．  
The following shows operations in the `Command`.  

* `Insert Path a` : `Path`の位置に`a`を挿入する  (Insert `a` at the position pointed by `Path`)
* `Delete Path a` : `Path`の位置の部分木を削除する．`a`は操作変換の実装に合わせるために存在しており，  
`a`に何を入れても動作には影響しない．`Main.hs`では`'___'`をダミーとして使用(Delete subtree at `Path`．
`a` exists to align with the implementation of operational transformation,
so the value of `a` does not affect the behavior of the operation in any way.
In `Main.hs`, `'___'` is used as a dummy value)
* `EditLabel Path a` : `Path`の位置のラベルを`a`に更新する (Update the label at position `Path` to  `a`)
* `Stay` : 何も編集しない (Do nothing)

### 使用例 (Usage example)
次のようなコマンドを用意します．  
Prepare the following commands.  

```
cmd1, cmd2, cmd3 :: Command Val
cmd1 = Insert [0,1] (read "{'x', 'y':[]}")
cmd2 = Delete [0,2] (read "'___'")
cmd3 = EditLabel [0,3] (read "'abc'")
```

ビューを生成します．  
Create the views as follows.  

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
`v1,v2` are the `State`s that contain the view as verbatim extraction of the content of the source.  

次に，それぞれのビューに異なる更新を加え，mputを実行します．  
Then, apply different updates for each view and execute mput.  

```
s' = extract $ editorMPutXML [(v1, cmd1), (v2, cmd2)]
```

`s'`は，`v1,v2`の更新内容が競合解決されたソースになります．  
`s'` will be the new source with the conflict caused by the updates on `v1` and `v2` are resolved.  

### Main.hs
このファイルで定義されている`main`はテスト用の3つの関数を選択できるようになっています．  
In the `main` function, you can choose one of the following three functions defined in this file for testing.

* `mputtest` : get -> mput -> get -> mput の順で実行する．get -> mputの時に必ず編集操作をはさむ
(execute get -> mput -> get -> mput in this order. Make sure an editing operation is inserted between get and mput)
* `putgetput` : mput -> get -> mputがPUTGETPUTを満たしているかを確認する
(check if the sequence of mput -> get -> mput satisfies PUTGETPUT)
* `getputget` : get -> mput -> getがGETPUTGETを満たしているかを確認する．
(check if the sequnence of get -> mput -> get satisfies GETPUTGET)
## 関数 (important functions)
### XEditor(app/EditorInf.hs)

#### editorGetXML
```
type XMLState = (Content, Inv Val, Content)
editorGetXML :: XMLState -> Either (Err (Inv Val) Val) XMLState
```
XMLStateは(XMLソース，ファンクション，XMLビュー)をもつタプルです．  
XMLState is a triple consisting of XML soure, function and XML view.  
Contentは[xml-1.3.14](https://hackage.haskell.org/package/xml-1.3.14)を使用しています．  
Content uses [xml-1.3.14](https://hackage.haskell.org/package/xml-1.3.14).  

#### editorMPut
```
type State = (Val, Inv Val, Val)

editorMPut :: [(State, Command Val)] -> Either (Err (Inv Val) Val) Val
```
Stateは(ソース, ファンクション, ビュー)をもつタプルです．  
State is a triple consisting of source, function and view.  
editorMPutは，ビュー編集者の数を要素数とし，各々の編集者がValに対して行った編集操作`Command Val`
を持つ，`(State, Command Val)`のリストを入力とします．  
editorMPut takes as input the list of `(State, Command Val)` which contains the same number
of elements as the number of editors of the view, where `Command Val` is the editing operations
on Val conducted by each editor.  
出力は，競合解決後の編集操作が適用されたソースです．  
The output is the source on which the editing operation after conflict resolution is applied.

#### editorMPutXML
```
editorMPutXML :: [(XMLState, Command Val)] -> Either (Err (Inv Val) Val) [XMLState]
```
editorMPutXMLは，ビュー編集者の数を要素数とし，各々の編集者がXMLに対して行った編集操作`Command Val`
を持つ，`(State, Command Val)`のリストを入力とします．  
editorMPutXML takes as input the list of `(State, Command Val)` which contains the
same number of elements as the number of editors of theview, where Command Val` is 
the editing opeeratons applied on XML by each each editor.  
出力は，各編集者のXMLStateを更新したものです．  
The output is the updated XMLState for each editor.
