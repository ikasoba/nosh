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

# pwsh の親戚を作った話

---

# 自己紹介

<div class="x1.25">

- いかそば

  - twitter.com/ikasoba000

- 好きなもの

  - PowerShell
  - TS, Haxe
  - 犬

- こういうのは初めてで緊張しますが がんばります！

<div>

---

<!-- _class: center -->

# なぜ作ろうと思ったのか

---

<div class="x1.25 center" style="flex-direction: column;">

`PowerShell` で `expressjs` のようなフレームワークを
書いているときにふと思ったんです・・・

</div>

---

<div class="center" style="position: fixed; top: 0; left: 0; z-index: 1;">
  <h1 style="color: red; background-color: white; border-radius: 512px; padding: 0.8rem;">
    「PowerShellでプログラム書くのはつらい！」
  </h1>
</div>

<div style="position: fixed; bottom: 2rem; font-size: 1.5rem">

- 動的スコープのせいでクロージャーを書くのがつらい！
- `Export-ModuleMember` で `class` や `enum` のエクスポートができない！
- 非同期系が発達してない！
  - C#でいろいろ書くことでどうにかなるが、
    イベントループもどきを実装しないとランタイムエラーが起きてしまう
- `Resolve-Path` はパスが存在してないとエラーになってしまうのがつらい！
- `.psd1`とか`.psm1`とかなんだよ！！！
  - （`.psd1`は`package.json`、`.psm1`は`.mjs`みたいなやつだよ）
    `.psm1`は`.ps1`でいいじゃん！！
    拡張子の最後の`1`ってなんだよ！！
- **そもそもシェルでそんなことするな！！！**

など…

</div>

---

# 機能

- 他のシェルと似たような構文
- シェルでのプログラムを書きやすく
  - 静的スコープの採用、dotnet との連携など

```sh
# 実行可能ファイルや関数の実行
> ls ()
> hoge-command ...
> 1.25.ToString

# 四則演算とか
> 1 + 2 * 3 - 4 == 3
```

---

# 今後の目標

- :black_square_button: パイプライン演算子
- :black_square_button: リダイレクト演算子
- :black_square_button: 静的型付け
- :black_square_button: LSP 対応

## 実装済み

- :white_check_mark: 実行可能ファイルの実行
- :white_check_mark: 関数の定義
- :white_check_mark: 関数の実行
- :white_check_mark: 四則演算と論理演算
