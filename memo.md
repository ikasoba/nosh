---
marp: true
---

<style>
  section {
    display: block;
  }

  section.center, .center {
    display: flex;
    justify-content: center;
    align-items: center;
    height: 100%;
    width: 100%;
  }

  .x1\.25 {
    font-size: 1.25rem;
  }

  .x2 {
    transform: scale(2);
  }
</style>

<!--
  _class: center
-->

# pwsh の親戚を作った(作ってる)話

---

# 自己紹介

<div class="x1.25">

- いかそば

  - twitter.com/ikasoba000

- 好きなもの

  - PowerShell
  - JS, TS, Haxe, aiscript など
  - 犬

- こういうのは初めてで緊張しますが がんばります！

<div>

---

# なぜ作ろうと思ったのか

<div class="x1.25 center" style="flex-direction: column;">

`PowerShell` で `expressjs` のようなフレームワークを
書いているときにふと思ったんです・・・

</div>

---

<div class="center" style="flex-direction: column;">

<h1>PowerShellでコード書きたくない！！</h1>

- **動的スコープのせい**でクロージャーを書くと魔境になる
- `Export-ModuleMember` で `class` や `enum` のエクスポートができない…。
- 非同期系があまり発達してない
  - Task の await ができないなど
  - C#でちょこっと手助けしてくれるコードを書けばなんとかなるけど…。

<br/>
なので・・・

</div>

---

<div class="center" style="flex-direction: column;">
  <h1>シェルを作ることにした！</h1>
</div>

---

# どんなものをつくったのか

静的スコープで普通の言語とシェルを融合させた感じの構文（ほぼ pwsh）

```
> $x = "Hello, World!"
Hello, World!
> echo $x
Hello, World!
```

---

# 今後の目標

- :black_square_button: 静的型付け
- :black_square_button: LSP 対応

その他いろいろ！！！

# 実装済み

- :white_check_mark: 実行可能ファイルの実行
- :white_check_mark: 関数の定義
- :white_check_mark: 関数の実行
- :white_check_mark: 四則演算と論理演算
- :white_check_mark: パイプライン演算子

---

# おわりに

<div class="x1.25 center" style="flex-direction: column;">
今回はPowerShellに不満を少しずつ感じ、<br/>
新しくシェルを作るまでをスライドにしてみました。<br/>
まだ機能は足りない物が多いですが、<br/>
少しずつできることを増やしていこうと思います。

<br/><br/>

ありがとうございました。ｍ（＿　＿）ｍ

</div>
