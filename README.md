# BalanceR 0.2.1

Author/Maintainer: Jaehyun Song (http://www.jaysong.net / tintstyle@gmail.com)

実験データのバランスチェック (修正：2019年5月31日)

開発途上のパッケージです。不具合は改善点など、歓迎致します。

*更新履歴*

* 20190531
  1. 実験群が2つの場合、`print()`関数の列名が正しく表示されない問題を修正しました。
  2. `tibble`オブジェクトで動かなかった問題を修正しました。
* 20190508: `plot()`関数に`color =`引数を追加しました。デフォルトは`TRUE`ですが、`FALSE`に設定すると白黒に表示されます。[善教将大](https://zkun.sakura.ne.jp)先生からご意見いただきました。

---
## Standardized Biasについて

実験群として統制群 (Control)と処置群 (Treat)がある場合、共変量Xの標準化差分は以下のように計算できます。

<center><img src="https://latex.codecogs.com/png.latex?\bg_white&space;\textrm{SB}&space;=&space;100&space;\cdot&space;\frac{\bar{X}_{\textrm{Treat}}&space;-&space;\bar{X}_{\textrm{Control}}}{\sqrt{0.5&space;\cdot&space;(s^2_{\textrm{Treat}}&space;&plus;&space;s^2_{\textrm{Control}})}}" title="\textrm{SB} = 100 \cdot \frac{\bar{X}_{\textrm{Treat}} - \bar{X}_{\textrm{Control}}}{\sqrt{0.5 \cdot (s^2_{\textrm{Treat}} + s^2_{\textrm{Control}})}}" /></center>

Xバーは平均値、s二乗は分散を意味します。ちなみにダミー変数の場合、以下のようになります。(`BalanceR`では変数が0/1のみで構成されている場合、この式で標準化差分を計算します。ただし、二値変数であっても、1/2のみで構成されている場合は該当しないので、データクリーニングの段階でダミー化をやっておくことをおすすめします。)

<center><img src="https://latex.codecogs.com/png.latex?\bg_white&space;\textrm{SB}&space;=&space;100&space;\cdot&space;\frac{\bar{X}_{\textrm{Treat}}&space;-&space;\bar{X}_{\textrm{Control}}}{\sqrt{0.5&space;\cdot&space;((\bar{X}_{\textrm{Treat}}&space;\cdot&space;(1&space;-&space;\bar{X}_{\textrm{Treat}})&space;&plus;&space;(\bar{X}_{\textrm{Control}}&space;\cdot&space;(1&space;-&space;\bar{X}_{\textrm{Control}}))}}" title="\textrm{SB} = 100 \cdot \frac{\bar{X}_{\textrm{Treat}} - \bar{X}_{\textrm{Control}}}{\sqrt{0.5 \cdot ((\bar{X}_{\textrm{Treat}} \cdot (1 - \bar{X}_{\textrm{Treat}}) + (\bar{X}_{\textrm{Control}} \cdot (1 - \bar{X}_{\textrm{Control}}))}}" /></center>

分野によって基準は変わりうると思いますが、標準化差分の絶対値が3, 5, 10未満ならバランスが取れていると判断します。社会科学では見る例だと10が多いような気がします。

---

## インストール

```
devtools::install_github("JaehyunSong/BalanceR")
```

---

## 使い方

```
# ダミーデータの読み込み
data(BlcDF)

# 回答者の性別 (Sex)、年齢 (Age)、教育水準 (Educ)、結婚有無 (Marriae)のバランスチェック
# 実験群を示す変数は"Group"
BlcChk <- BalanceR(data = BlcDF, group = "Group",
                   cov  = c("Sex", "Age", "Educ", "Marriage"))

print(BlcChk, digits = 3)
```

以上の関数を走らせると以下のような結果が出ます。

```
  Covariate Mean:Control SD:Control Mean:Treat1 SD:Treat1 Mean:Treat2
1       Sex        0.391      0.488       0.403     0.491       0.408
2       Age       41.941      9.863      41.062     9.952      41.688
3      Educ        3.213      0.902       3.245     0.888       3.226
4  Marriage        0.438      0.496       0.411     0.492       0.402
  SD:Treat2 SB:Control-Treat1 SB:Control-Treat2 SB:Treat1-Treat2
1     0.492            -2.507            -3.527           -1.021
2     9.366             8.912             2.561           -6.295
3     0.901            -3.587            -1.466            2.154
4     0.491             5.458             7.243            1.783
```

`magrittr`パッケージやその他パイプ演算子(`%>%`)を用いるパッケージ (`dplyr`、`ggplot2`など)が読み込まれているなら、パイプ演算子も使えます。

```
library(magrittr)

BlcDF %>%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage"))
```

結果画面のカスタマイズは現在のところ、小数点の桁数 (`digits`)と標準化差分のみ表示 (`only.SB`)です。

デフォルトは`digits = 3`、`only.SB = FALSE`となります。

```
## 結果を小数点4桁まで表示させる
BlcDF %>%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) %>%
    print(digits = 4)

## Standardized biasのみ表示させる
BlcDF %>%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) %>%
    print(only.SB = TRUE)
```

各共変量の平均値・標準偏差が必要ない場合、以上のように実行すると以下のように結果が返ってきます。

```
  Covariate SB:Control-Treat1 SB:Control-Treat2 SB:Treat1-Treat2
1       Sex            -2.507            -3.527           -1.021
2       Age             8.912             2.561           -6.295
3      Educ            -3.587            -1.466            2.154
4  Marriage             5.458             7.243            1.783
```

---

可視化も可能です。カスタマイズ可能な部分は垂直線の位置 (`vline`)、点の大きさ (`point.size`)、文字の大きさ (`text.size`)のみです。

デフォルトは`vline = c(3, 5, 10)`、`point.size = 2.5`、`text.size = 12`です。

```
## プロットのみ表示
BlcDF %>%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) %>%
    plot()
```
![](https://github.com/JaehyunSong/BalanceR/blob/master/Screenshot/Plot1.png)

```
## 垂直線を引く
BlcDF %>%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) %>%
    plot(vline = c(10))
```

![](https://github.com/JaehyunSong/BalanceR/blob/master/Screenshot/Plot2.png)

```
## 点と文字の大きさを変更
BlcDF %>%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) %>%
    plot(point.size = 5, text.size = 18)
```

![](https://github.com/JaehyunSong/BalanceR/blob/master/Screenshot/Plot3.png)

白黒に出力する場合、`color = FALSE`を指定してください。
```
## 白黒に変更
BlcDF %>%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) %>%
    plot(point.size = 5, text.size = 18, color = FALSE)
```

![](https://github.com/JaehyunSong/BalanceR/blob/master/Screenshot/Plot5.png)

図はggplot2で作成されているので、`+`で繋げれば、自由にカスタマイズできます。

```
## もっともっとカスタマイズしたい
BlcDF %>% 
    BalanceR(group = "Group", 
             cov   = c("Sex", "Age", "Educ", "Marriage")) %>% 
    plot(point.size = 5, text.size = 18) +
    labs(x = "標準化差分", y = "共変量") +
    theme_bw(base_family = "HiraKakuProN-W3") +
    theme(legend.position = "bottom",
          text = element_text(size = 18))
```
![](https://github.com/JaehyunSong/BalanceR/blob/master/Screenshot/Plot4.png)
