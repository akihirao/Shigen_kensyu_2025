## 各種ライブラリーの読み込み
library(KFAS)
library(tidyverse)
library(lubridate)
library(forecast)

library(gridExtra)


##作業場所の設定
cur_dir <- getwd()
#print(cur_dir)
setwd(cur_dir)

# initialize
rm(list=ls(all=TRUE))

SST_monthly_df2ts <- function(SST_monthly_df){
  start_year_month <- min(SST_monthly_df$Time)

  # ts型に変換
  SST_ts <- SST_monthly_df %>% 
    dplyr::select(-Time) %>%             # 日付列と秋冬フラグを削除
    ts(start = c(year(start_year_month), month(start_year_month)), 
      frequency = 12) # ts型に変換(1998年1月開始。12か月1周期)
}


#沿岸域の海面水温情報
#https://www.data.jma.go.jp/kaiyou/data/db/kaikyo/series/engan/engan.html
#上記のサイトから、対象の海域番号を調べる
#（例えば、岩手県南部沿岸の海域番号は113)


# 水温データのソースの指定: local, url, or, original
source <- "local" # local (default), url, or original
# local:　local環境にある提供データ: ulr: 気象庁公開のデータ; original: 自身が用意されたオリジナルデータ


if(source=="url"){ #ウェブサイトから海面水温データを読み込む場合
  sea_are_id <- 113　#岩手県南部沿岸海域の例
  url <- paste0("https://www.data.jma.go.jp/kaiyou/data/db/kaikyo/series/engan/txt/area",
                sea_are_id,
                "-past.txt")

  SST_info <- read_csv(url) %>% 
    rename(Temp="Temp.") %>% 
    mutate(date=as.Date(paste0(yyyy,"-",mm,"-",dd))) %>% 
    select(c(date,Temp,flag))
  
  #　SSTを月別平均に集約
  SST_monthly_avg <- SST_info %>% 
    mutate(Time = floor_date(date, "month")) %>% 
    group_by(Time) %>% 
    summarise(Temp = mean(Temp, na.rm = TRUE))

  SST_ts <- SST_monthly_df2ts(SST_monthly_avg)
  
  
}else if(source=="local"){ #ローカルにある海面水温データファイルを読み込む場合
  SST_info <- read_csv("SST_area133.csv") %>%  #岩手県南部沿岸海域の海面水温データ（デフォルト）
    rename(Temp="Temp.") %>% 
    mutate(date=as.Date(paste0(yyyy,"-",mm,"-",dd))) %>% 
    select(c(date,Temp,flag))
  
  #　SSTを月別平均に集約
  SST_monthly_avg <- SST_info %>% 
    mutate(Time = floor_date(date, "month")) %>% 
    group_by(Time) %>% 
    summarise(Temp = mean(Temp, na.rm = TRUE))
  
  SST_ts <- SST_monthly_df2ts(SST_monthly_avg)
  
}else if(source=="original"){ #ローカルに置いたオリジナルの海面水温データファイルを読み込む場合
  year_month_temp <- read_csv("SST_original.csv")
  
  SST_ts <- ts(year_month_temp$Temp,
               start = c(year_month_temp$Year[1], year_month_temp$Month[1]),
               frequency = 12) # ts型に変換(12か月1周期)
  names(SST_ts) <- "Temp"

}else{
  
}

head(SST_ts)


frequency(SST_ts)   # 周波数（12）
start(SST_ts)       # 開始（1982, 1）
end(SST_ts)         # 終了（2025, 10）
cycle(SST_ts)       # 各観測の月番号（1～12）
time(SST_ts)        # 小数年（1982.000, 1982.083...）
window(SST_ts, start = c(1991, 1), end = c(2020, 12))  # 期間抽出

stopifnot(frequency(SST_ts) == 12)  # 月次であることの確認

temp <- as.numeric(SST_ts)

# 月番号（1〜12）を取得
#mnum <- factor(cycle(SST_ts),levels = 1:12)
mnum <- cycle(SST_ts)

# 念のため 1:12 の factor に固定（欠月があっても 12要素の平均が返る）
mfac <- factor(mnum, levels = 1:12)

# 3) 月別の長期平均（気候値）を作る
monthly_mean <- tapply(temp, mfac, function(v) mean(v, na.rm = TRUE))

# 各観測に対応する月平均を展開
clim <- monthly_mean[as.integer(mfac)]

# 4) アノマリー（海水温偏差）を計算
anom <- temp - clim


SST_dev_ts <- cbind(Temp=SST_ts, Temp_dev=anom)

head(SST_dev_ts)

# 時系列折れ線グラフ
SST_ts_plot <- autoplot(SST_dev_ts[,"Temp"]) +
  labs(y = "Temperature (℃)", x = "Time") +
  ggtitle("Sea surface temperature")

SST_dev_ts_plot <- autoplot(SST_dev_ts[,"Temp_dev"]) +
  labs(y = "Temperature (℃)", x = "Time") +
  ggtitle("Sea surface temperature anomalies")

# 並べる
gridExtra::grid.arrange(SST_ts_plot, 
                        SST_dev_ts_plot,
                        ncol = 1)

make_ssm_SST <- function(ts_data) {
  # モデルの構造を決める
  build_ssm <- SSModel(
    H = NA,
    Temp ~
      SSMtrend(degree = 2,                  # 平滑化トレンドモデル
               Q = c(list(0), list(NA))) +
      SSMseasonal(
        sea.type = "dummy", # ダミー変数を利用した季節成分
        period = 12,        # 周期は12とする
        Q = NA
      ) +
      SSMarima(
        ar = c(0, 0),       # 2次のAR成分
        d = 0,
        Q = 0
      ),
    data = ts_data
  )
  
  # optimに渡す前にパラメータをexpしたりartransformしたり、変換する
  # ほぼbuild_ssmと同じだが、パラメータだけ変更されている
  update_func <- function(pars, model) {
    model <- SSModel(
      H = exp(pars[6]),
      Temp ~
        SSMtrend(degree = 2,
                 Q = c(list(0), list(exp(pars[1])))) +
        SSMseasonal(
          sea.type = "dummy",
          period = 12,
          Q = exp(pars[2])
        ) +
        SSMarima(
          ar = artransform(pars[3:4]),
          d = 0,
          Q = exp(pars[5])
        ),
      data = ts_data
    )
  }
  
  
  # 最適化その1。まずはNelder-Mead法を用いて暫定的なパラメータを推定
  fit_ssm_bef <- fitSSM(
    build_ssm,
    inits = c(-17,-30, 0.5, 0, -1, -3), # パラメータの初期値(任意)
    update_func,
    method = "Nelder-Mead",
    control = list(maxit = 5000, reltol = 1e-16)
  )
  
  # 最適化その2。先ほどの結果を初期値に使ってもう一度最適化する
  fit_ssm <- fitSSM(
    build_ssm,
    inits = fit_ssm_bef$optim.out$par,
    update_func,
    method = "BFGS",
    control = list(maxit = 5000, reltol = 1e-16)
  )
  
  # フィルタリングとスムージング
  result_ssm <- KFS(
    fit_ssm$model,
    filtering = c("state", "mean"),
    smoothing = c("state", "mean", "disturbance")
  )
  
  # 結果の出力
  return(list(fit_ssm, result_ssm))
  
}

list_SST <- make_ssm_SST(SST_dev_ts)
fit_SST    <- list_SST[[1]]
result_SST <- list_SST[[2]]


# 推定結果 -------------------------------------------------------------

# 平滑化推定量
head(result_SST$alphahat)


# 係数の95%信頼区間
res <- confint(result_SST, level = 0.95)
#res[c("Kuroshio")] %>% lapply(head, n = 1)


# 状態の可視化
#autoplot(
#  result_kuroshio$alphahat[, c("level", "slope", "sea_dummy1", "arima1")],
#  facets = TRUE
#)

model_level_plot <- autoplot(result_SST$alphahat[,"level"]) +
  labs(y = "", x = "Time") +
  ggtitle("Level component")

model_slope_plot <- autoplot(result_SST$alphahat[,"slope"]) +
  labs(y = "", x = "Time") +
  ggtitle("Drift component")

model_season_plot <- autoplot(result_SST$alphahat[,"sea_dummy1"]) +
  labs(y = "", x = "Time") +
  ggtitle("Seasonal component")

model_arima1_plot <- autoplot(result_SST$alphahat[,"arima1"]) +
  labs(y = "", x = "Time") +
  ggtitle("Auto-regression component")


model_out_plot <- grid.arrange(model_level_plot,
                              model_slope_plot,
                              model_season_plot,
                              model_arima1_plot,
                              ncol = 1)


knitr::purl("SST_ts_analyses.Rmd", 
            output ="SST_ts_analyses.r",
            documentation = 0)
