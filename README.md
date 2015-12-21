# 日頃よく使う関数群

## How to use

#### Graphtec.R, MCH.R
データロガー (GLシリーズ, Graphtech) と温湿度CO2濃度計 (MCH-383SD, 佐藤商事)のデータをまとめます。  
詳しくは<a>リンク</a>参照。

#### labels.R
ggplot2で使うラベル。  
expressionを使って下付き上付き、ギリシャ文字など。  

#### phytochrome.R
照射光の分光分布からフィトクロム平衡値を計算します。  
Calculate phytochrome photostationally state according to the spectral distribution of the light and [Sager et al. (1988) (pdf download link)]("https://elibrary.asabe.org/azdez.asp?JID=3&AID=30952&ConfID=t1988&v=31&i=6&T=2").  

#### summariser.R
data.frameから統計量 (samplesize, mean, SD, SE) とTukey-testの結果を計算します。  
検定は第一カラムをグループとして実施。  
group_by() %>% summarise_each(funs(mean, sd, se, length))  
with Tukey' test by the first column.   