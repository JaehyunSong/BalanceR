# BalanceR
Song Jaehyun (http://www.jaysong.net / tintstyle@gmail.com)

実験データのバランスチェック (修正：2019年5月7日)

開発途上のパッケージです。不具合は改善点など、歓迎致します。

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

# 回答者の性別 (Sex)、年齢 (Age)、教育水準 (Educ)、結婚有無 (Marriate)のバランスチェック
# 実験群を示す変数は"Group"
BlcChk <- BalanceR(data = BlcDF, group = "Group",
                   cov  = c("Sex", "Age", "Educ", "Marriage"))

print(BlcChk)
```

以上の関数を走らせると以下のような結果が出ます。一般的に、Standardized Bias (SB)が10未満ならバランスが取れていると解釈します。他にも3か5を用いる場合もあります。

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

パイプ演算子(`%>%`)も使えます。

```
BlcDF %>%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage"))
```

結果画面のカスタマイズは現在のところ、小数点の桁数 (`digits`)とStandardized Baisのみ表示 (`only.SB`)です。

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
