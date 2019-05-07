# BalanceR
Song Jaehyun (http://www.jaysong.net)

修正：2019年5月7日

実験データのバランスチェック

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
BalanceR(data = BlcDF, group = "Group",
         cov  = c("Sex", "Age", "Educ", "Marriage"))
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
    print(only.SB = TRUE, digits = 3)
```

可視化も可能です。カスタマイズ可能な部分は垂直線の位置 (`vline`)、点の大きさ (`point.size`)、文字の大きさ (`text.size`)のみです。

デフォルトは`vline = c(3, 5, 10)`、`point.size = 2.5`、`text.size = 12`です。

```
## プロットのみ表示
BlcDF %>%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) %>%
    plot()
```

```
## 垂直線を引く
BlcDF %>%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) %>%
    plot(vline = c(10))
```

```
## 点と文字の大きさを変更
BlcDF %>%
    BalanceR(group = "Group",
             cov   = c("Sex", "Age", "Educ", "Marriage")) %>%
    plot(point.size = 5, text.size = 18)
```
