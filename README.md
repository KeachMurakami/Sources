# 日頃よく使う関数群

## How to use
---------
#### ggplot_accessory.R
ggplot2 (ver.2~) で表示周りを自分好みに。  
出力を論文っぽくすることが主目的。

#### Graphtec.R, MCH.R
データロガー (GLシリーズ, Graphtech) と温湿度CO2濃度計 (MCH-383SD, 佐藤商事)のデータをまとめます。  
詳しくは<a>リンク</a>参照。

#### labels.R
ggplot2で使うラベル。  
expressionを使って下付き上付き、ギリシャ文字など。  

#### phytochrome.R
照射光の分光分布からフィトクロム平衡値を計算します。  
詳細は原著 [Sager et al. 1988](https://www.google.co.jp/search?q=Sager+1988+%22Photosynthetic+Efficiency+and+Phytochrome+Photo+Equilibrium+Determination+Using+Spectral+Data%22&oq=Sager+1988+%22Photosynthetic+Efficiency+and+Phytochrome+Photo+Equilibrium+Determination+Using+Spectral+Data%22&aqs=chrome..69i57j69i60.225j0j4&sourceid=chrome&es_sm=91&ie=UTF-8) を読んでください。  
フィトクロム光断面積のデータはwebからフリーで取得できる模様。  

Calculate phytochrome photostationally state according to the spectral distribution of the light and [Sager et al. (1988)](https://www.google.co.jp/search?q=Sager+1988+%22Photosynthetic+Efficiency+and+Phytochrome+Photo+Equilibrium+Determination+Using+Spectral+Data%22&oq=Sager+1988+%22Photosynthetic+Efficiency+and+Phytochrome+Photo+Equilibrium+Determination+Using+Spectral+Data%22&aqs=chrome..69i57j69i60.225j0j4&sourceid=chrome&es_sm=91&ie=UTF-8).  

#### standard_funs.R
sum, and, meanのna.rm = TRUEバージョンやreshape2::meltをカラム番号で実施する関数など。  
使う場面が多い。

#### standard_funs.R
起動時読み込みをMacとWinで統一。
それぞれの環境の`Rprofile.site`にはこれを読みこませるだけでOK。

#### summariser.R
data.frameから統計量 (samplesize, mean, SD, SE) とTukey-testの結果を計算します。  
検定は第一カラムをグループとして実施。  
group_by() %>% summarise_each(funs(mean, sd, se, length))  
with Tukey' test by the first column.   