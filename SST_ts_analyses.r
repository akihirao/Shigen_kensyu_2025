#' ---
#' title: "SST_trend_analysis"
#' author: "Akira Hirao"
#' date: "`r Sys.Date()`"
#' output:
#'   md_document:
#'     toc: true
#'     variant: markdown_github
#'   html_document:
#'     toc: true
#' ---
#' 
#' # 解析環境の設定
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
## 各種ライブラリーの読み込み
library(KFAS)
library(tidyverse)
library(lubridate)
library(forecast)
# あとで使うautoplot関数はtidyverseとforecast両方に存在して競合するので、必ず上の順番でライブラリを読み込んでください！
library(gridExtra)


# initialize
rm(list=ls(all=TRUE))

#' 
#' # 海面水温時系列データの読み込み
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
SST_monthly_df2ts <- function(SST_monthly_df){
  start_year_month <- min(SST_monthly_df$Time)

  # ts型に変換
  SST_ts <- SST_monthly_df %>% 
    dplyr::select(-Time) %>%             # 日付列と秋冬フラグを削除
    ts(start = c(year(start_year_month), month(start_year_month)), 
      frequency = 12) # ts型に変換(1998年1月開始。12か月1周期)
  return(SST_ts)
}


#沿岸域の海面水温情報
#https://www.data.jma.go.jp/kaiyou/data/db/kaikyo/series/engan/engan.html
#上記のサイトから、対象の海域番号を調べる
#（例えば、岩手県南部沿岸の海域番号は113)


# 水温データのソースの指定: local, url, or, original
source <- "local" # local (default), url, or original
# local:　local環境にある提供データ: ulr: 気象庁公開のデータ; original: 自身が用意されたオリジナルデータ

# 海域番号、海域名の対応表
# https://www.data.jma.go.jp/kaiyou/data/db/kaikyo/series/engan/eg_areano.html
# 例
# 122: 釧路地方沿岸
# 133: 岩手県南部沿岸
# 315: 佐渡島
# 321: 福井県沿岸
# 709: 与那国島

if(source=="url"){ #ウェブサイトから海面水温データを読み込む場合
  sea_area_id <- 138 #海域番号
  url <- paste0("https://www.data.jma.go.jp/kaiyou/data/db/kaikyo/series/engan/txt/area",
                sea_area_id,
                ".txt")

  #読み込みデータの列名依存の確認 
  tmp <- read_csv("url")
  message("Columns in SST file: ", paste(names(tmp), collapse=", "))
  
  SST_info <- read_csv(url) %>% 
    slice(-n()) %>%
    rename(Temp="Temp.") %>% 
    mutate(Temp = as.numeric(Temp)) %>%
    mutate(date=as.Date(paste0(yyyy,"-",mm,"-",dd))) %>% 
    select(c(date,Temp,flag)) %>% 
    filter(date <= as.Date("2025-12-31"))
  
  #　SSTを月別平均に集約
  SST_monthly_avg <- SST_info %>% 
    mutate(Time = floor_date(date, unit = "month")) %>% 
    group_by(Time) %>% 
    summarise(Temp = mean(Temp, na.rm = TRUE))

  SST_ts <- SST_monthly_df2ts(SST_monthly_avg)
  SST_ts <- window(SST_ts, end = c(2025,12))
  
  
}else if(source=="local"){ #ローカルにある海面水温データファイルを読み込む場合
   
  #読み込みデータの列名依存の確認 
  tmp <- read_csv("SST_area138.csv")
  message("Columns in SST file: ", paste(names(tmp), collapse=", "))
  
  #岩手県南部沿岸海域の海面水温データ（デフォルト）を読み込む
  SST_info <- read_csv("SST_area138.csv") %>%  
    rename(Temp="Temp.") %>% 
    mutate(date=as.Date(paste0(yyyy,"-",mm,"-",dd))) %>% 
    select(c(date,Temp,flag))
  
  #　SSTを月別平均に集約
  SST_monthly_avg <- SST_info %>% 
    mutate(Time = floor_date(date, unit = "month")) %>% 
    group_by(Time) %>% 
    summarise(Temp = mean(Temp, na.rm = TRUE))
  
  SST_ts <- SST_monthly_df2ts(SST_monthly_avg)
  SST_ts <- window(SST_ts, end = c(2025,12))
  
}else if(source=="original"){ #ローカルに置いたオリジナルの海面水温データファイルを読み込む場合
  year_month_temp <- read_csv("SST_original.csv")
  
  SST_ts <- ts(year_month_temp$Temp,
               start = c(year_month_temp$Year[1], year_month_temp$Month[1]),
               frequency = 12) # ts型に変換(12か月1周期)
  names(SST_ts) <- "Temp"

}else{
  
}


head(SST_ts)

#' 
#' # tsオブジェクトの内容の確認
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------

frequency(SST_ts)   # 周波数（12）
start(SST_ts)       # 開始（1982, 1）
end(SST_ts)         # 終了（2025, 10）
cycle(SST_ts)       # 各観測の月番号（1～12）
time(SST_ts)        # 小数年（1982.000, 1982.083...）
window(SST_ts, start = c(2000, 1), end = c(2005, 12))  # 期間抽出

#' 
#' # SSTの時系列折れ線グラフ
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
SST_ts_source_plot <- autoplot(SST_ts) +
  labs(y = expression(Temperature~(degree*C)), x = "Time") +
  ggtitle("Sea surface temperature")

plot(SST_ts_source_plot)

#' 
#' # SST偏差系列の作成
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
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

# 4) 海水温偏差を計算
anom <- temp - clim


SST_dev_ts <- cbind(Temp=SST_ts, Temp_dev=anom)

head(SST_dev_ts)

#' # 月平均SSTのプロット
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------

monthly_mean_fac_tidy = tibble(Month=factor(1:12,
                                        levels=(1:12)),
       SST=as.numeric(monthly_mean)
       )

monthly_mean_tidy = tibble(Month=(1:12),
       SST=as.numeric(monthly_mean)
       )

head(monthly_mean_tidy)
summary(monthly_mean_tidy)

plot_monthly_mean_SST <- ggplot(data=monthly_mean_fac_tidy,
                                 aes(x=Month,y=SST)
                                 ) +
  geom_point(size = 1) + 
  geom_line(data=monthly_mean_tidy,
            aes(x=Month,y=SST),linetype= "dashed") + 
  scale_x_discrete(
    labels = function(x) sprintf("%02d", as.integer(x))
  )
  
plot_monthly_mean_SST


#' 
#' # SST偏差系列のプロット：autoplot
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
forecast::autoplot(SST_dev_ts)

#' 
#' # SST偏差系列のプロット: ggplot
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
SST_ts_plot <- forecast::autoplot(SST_dev_ts[,"Temp"]) +
  labs(y = expression(Temperature~(degree*C)), x = "Time") +
  ggtitle("Sea surface temperature")

SST_dev_ts_plot <- forecast::autoplot(SST_dev_ts[,"Temp_dev"]) +
  labs(y = expression(Temperature~(degree*C)), x = "Time") +
  ggtitle("Sea surface temperature anomalies")

# 並べる
SST_ts_source_plot <- gridExtra::grid.arrange(SST_ts_plot,
                                              SST_dev_ts_plot,
                                              ncol = 1)


plot(SST_ts_source_plot)

#' 
#' # 線形ガウス状態空間モデルの定義（外生変数なしモデル：Model 0）
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
make_ssm_M0 <- function(ts_data) {
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
    return(
    SSModel(
      H = exp(pars[6]),
      Temp ~
        SSMtrend(
          degree = 2,
          Q = c(list(0), list(exp(pars[1])))) +
        SSMseasonal(
          sea.type = "dummy",
          period = 12,
          Q = exp(pars[2])) +
        SSMarima(
          ar = artransform(pars[3:4]),
          d = 0,
          Q = exp(pars[5])),
      data = ts_data
      )
    )
  }
  
  
  # 最適化その1。まずはNelder-Mead法を用いて暫定的なパラメータを推定
  fit_ssm_bef <- fitSSM(
    build_ssm,
    #inits = c(-17,-30, 0.5, 0, -1, -3), # パラメータの初期値(任意)
    inits = c(-13,-7, 0.9, -0.1, -0.3, -5), # パラメータの初期値(任意)
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

#' 
#' # M0モデル関数の適用
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
list_M0 <- make_ssm_M0(SST_dev_ts)
fit_M0    <- list_M0[[1]]
result_M0 <- list_M0[[2]]

# 係数の95%信頼区間
res_M0 <- confint(result_M0, level = 0.95)

#' 
#' # M0モデルの推定結果の確認
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
#モデル推定の収束の確認
fit_M0$optim.out$convergence

par_M0 <- fit_M0$optim.out$par #モデルの推定パラメーター
par_comp_M0 <- c(Q_trend  = exp(par_M0[1]), # 年トレンドの大きさ
                  Q_season = exp(par_M0[2]), # 季節トレンドの大きさ
                  AR1      = artransform(par_M0[3:4])[1], # 1次のARの大きさ
                  AR2      = artransform(par_M0[3:4])[2], # 2次のARの大きさ
                  Q_ar     = exp(par_M0[5]), # 短期変動の揺らぎ
                  H        = exp(par_M0[6])) # 観察誤差の大きさ
par_comp_M0

# 平滑化推定量
alpha_hat_M0 <- result_M0$alphahat
head(alpha_hat_M0)

level_M0 <- alpha_hat_M0[,"level"]
drift_M0 <- alpha_hat_M0[,"slope"]

level_M0_ts <- ts(level_M0, start = start(SST_ts), frequency = 12)
drift_M0_ts <- ts(drift_M0, start = start(SST_ts), frequency = 12)

# 馬場ら（2024)との比較のため, 2023年2月のドリフト成分の抽出
drift_M0_2023Feb <- window(drift_M0_ts, start=c(2023, 2), end=c(2023, 2))

# 年あたりに換算した2023年2月時点の瞬間的な昇温率：報告値その１
drift_M0_2023Feb_per_year <- drift_M0_2023Feb*12
print(drift_M0_2023Feb_per_year) 

# 年あたりの平均的な昇温率：報告値その2
mean_drift_year_M0 <- mean(drift_M0_ts) * 12
print(mean_drift_year_M0)


# 成分別にプロット
level_M0_plot <- autoplot(result_M0$alphahat[,"level"]) +
  labs(y = "", x = "Time") +
  ggtitle("Level component")

drift_M0_plot <- autoplot(result_M0$alphahat[,"slope"]) +
  labs(y = "", x = "Time") +
  ggtitle("Drift component")

season_M0_plot <- autoplot(result_M0$alphahat[,"sea_dummy1"]) +
  labs(y = "", x = "Time") +
  ggtitle("Seasonal component")

arima1_M0_plot <- autoplot(result_M0$alphahat[,"arima1"]) +
  labs(y = "", x = "Time") +
  ggtitle("Auto-regression component")


M0_out_plot <- grid.arrange(level_M0_plot,
                            drift_M0_plot,
                            season_M0_plot,
                            arima1_M0_plot,
                            ncol = 1)

plot(M0_out_plot)


#' 
#' # M0モデルの残差のチェック
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
# 標準化残差
std_obs_resid_M0 <- rstandard(result_M0, type = "recursive")

# forecastパッケージのcheckredisuals関数で残差のチェック
# Ljung–Box検定: P > 0.05で残差に有意な自己相関なしと判断
checkresiduals(std_obs_resid_M0)

# 図示された残差（上：残差系列；左下：残差コレログラム；右下：残差のヒストグラム）をみて異常に突出した残差がないかなどを確認

#正規性の確認
# P > 0.05で正規分布と有意に異なっていないと判断
shapiro.test(std_obs_resid_M0)


#' 
#' # 水準変動とその95%信頼区間の図示
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
level_M0_tidy <- cbind(
  data.frame(time=time(SST_ts),
             SST=as.numeric(SST_ts),
             level_M0=level_M0),
  as.data.frame(res_M0$level)
  ) %>%
  as_tibble() %>%
  rename(lwr_M0=lwr,upr_M0=upr)


level_M0_ggplot <- ggplot(data=level_M0_tidy,
                         aes(x=time,y=level_M0)) +
  labs(title="Level component",x="Year", y="SST") +
  geom_line(aes(y=level_M0), size = 1.2) +
  geom_ribbon(aes(ymin = lwr_M0, ymax = upr_M0), alpha = 0.3)

ggsave("level_M0_plot.png",
       width=6, height=4,
       plot = level_M0_ggplot)

level_M0_ggplot

#' 
#' # ドリフト成分とその95%信頼区間の図示
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
drift_M0_tidy <- cbind(
  data.frame(time=time(SST_ts),
             Temp=as.numeric(SST_ts),
             drift_M0=drift_M0),
  as.data.frame(res_M0$slope)
  ) %>%
  as_tibble() %>%
  rename(lwr_M0=lwr,upr_M0=upr)


annual_drift_M0_lab <- paste0("average annual drift = ",round(mean_drift_year_M0,3))

drift_M0_ggplot <- ggplot(data=drift_M0_tidy,
                         aes(x=time,y=drift_M0)) +
  labs(title=paste0("Drift component: ",annual_drift_M0_lab),
       x="Year", y="Drift") +
  geom_line(aes(y=drift_M0), size = 1.2) +
  geom_ribbon(aes(ymin = lwr_M0, ymax = upr_M0), alpha = 0.3) +
  geom_hline(yintercept=0, linetype="dashed") 

ggsave("drift_M0_plot.png",
       width=6, height=4,
       plot = drift_M0_ggplot)

drift_M0_ggplot

#' 
#' # 予測 ----------------------------------------------------------------------
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------

# n.ahead=6: ６時点先までを予測
forecast_pred_M0 <- predict(result_M0$model,
                         interval="prediction",
                         level = 0.95,
                         n.ahead = 6)

print(forecast_pred_M0)


#' # =============================================================================================
#' # 欠損データの解析 ----------------------------------------------------------------------
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
# 欠損データセットの作成
# 時系列データの半期から８年の観測値を欠損とする
n <- length(SST_ts)
half_point <- trunc(n/2)
NA_point <- seq(half_point, min(half_point + 96, n))
SST_NA_ts <- SST_ts
SST_NA_ts[NA_point] <- NA 

SST_NA_ts_source_plot <- autoplot(SST_NA_ts) +
  labs(y = expression(Temperature~(degree*C)), x = "Time") +
  ggtitle("Sea surface temperature")

plot(SST_NA_ts_source_plot)

#' 
#' # M0モデルの適用
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
list_M0_NA <- make_ssm_M0(SST_NA_ts)
fit_M0_NA <- list_M0_NA[[1]]
result_M0_NA <- list_M0_NA[[2]]


# 成分別にプロット
level_M0_NA_plot <- forecast::autoplot(result_M0_NA$alphahat[,"level"]) +
  labs(y = "", x = "Time") +
  ggtitle("Level component")

drift_M0_NA_plot <- forecast::autoplot(result_M0_NA$alphahat[,"slope"]) +
  labs(y = "", x = "Time") +
  ggtitle("Drift component")

season_M0_NA_plot <- forecast::autoplot(result_M0_NA$alphahat[,"sea_dummy1"]) +
  labs(y = "", x = "Time") +
  ggtitle("Seasonal component")

arima1_M0_NA_plot <- forecast::autoplot(result_M0_NA$alphahat[,"arima1"]) +
  labs(y = "", x = "Time") +
  ggtitle("Auto-regression component")


M0_NA_out_plot <- grid.arrange(level_M0_NA_plot,
                               drift_M0_NA_plot,
                               season_M0_NA_plot,
                               arima1_M0_NA_plot,
                               ncol = 1)

plot(M0_NA_out_plot)

#' 
#' 
#' # =============================================================================================
#' # 外生変数組み込みモデルの解析 ----------------------------------------------------------------
#' 
#' # 黒潮続流北限緯度データの読み込み
#' # https://ocean.fra.go.jp/temp/O-K.html
#' # 本データの2次利用配布は不可とさせていただきます。
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
#黒潮続流北限緯度データの読み込み
Kuroshio_df <- read_table("gknmay9.txt",
                          col_names=FALSE) %>%
  rename(Year="X1",
         Month="X2",
         Kuroshio="X4") %>%
  mutate(Time=as.Date(paste0(Year,"-",Month,"-01"))) %>%
  dplyr::select(Time, Kuroshio) %>%
  mutate(Kuroshio=na_if(Kuroshio, 999)) #黒潮続流データの欠損値999をNAに置換

Kuroshio_start_year <- min(Kuroshio_df$Time) %>% year()
Kuroshio_start_month <- min(Kuroshio_df$Time) %>% month()

Kuroshio_ts <- ts(Kuroshio_df$Kuroshio,
                  start=c(Kuroshio_start_year,
                          Kuroshio_start_month),
                  frequency = 12)

names(Kuroshio_ts) <- "Kuroshio"
 
start_Kuroshio_ts <- start(Kuroshio_ts)
end_Kuroshio_ts <- end(Kuroshio_ts)


start_SST_ts <- start(SST_dev_ts)
end_SST_ts <- end(SST_dev_ts)

# SSTと黒潮続流北限緯度の時系列データを重複期間で統合
SST_Kuroshio_ts  <- ts.intersect(Temp = SST_dev_ts[,"Temp"],
                                 Temp_dev = SST_dev_ts[,"Temp_dev"],
                                 Kuroshio=Kuroshio_ts,
                                 Kuroshio_scaled=scale(Kuroshio_ts) #標準化(平均０、標準偏差1)
                                 )


head(SST_Kuroshio_ts)

#' 
#' # SSTの時系列折れ線グラフ
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
# 時系列折れ線グラフ
SST_ts_plot2 <- forecast::autoplot(SST_Kuroshio_ts[,"Temp"]) +
  labs(y = expression(Temperature~(degree*C)), x = "Time") +
  ggtitle("Sea surface temperature")

SST_dev_ts_plot2 <- forecast::autoplot(SST_Kuroshio_ts[,"Temp_dev"]) +
  labs(y = expression(Temperature~(degree*C)), x = "Time") +
  ggtitle("Sea surface temperature anomalies")

Kuroshio_ts_plot <- forecast::autoplot(SST_Kuroshio_ts[,"Kuroshio"]) +
  labs(y = "Latitude (degree)", x = "Time") +
  ggtitle("North limit of Kuroshio extension")


# 並べてプロット
SST_Kuroshio_ts_plot <- gridExtra::grid.arrange(SST_ts_plot2, 
                        SST_dev_ts_plot2,
                        Kuroshio_ts_plot,
                        ncol = 1)

plot(SST_Kuroshio_ts_plot)

#' 
#' # 線形ガウス状態空間モデル(外生変数あり: M1）
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
make_ssm_M1 <- function(ts_data) {
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
      ) + 
      SSMregression(
        ~ Kuroshio_scaled, Q = 0
        ), # 外生変数
    data = ts_data
  )
  
  # optimに渡す前にパラメータをexpしたりartransformしたり、変換する
  # ほぼbuild_ssmと同じだが、パラメータだけ変更されている
  update_func <- function(pars, model) {
    return(
      SSModel(
        H = exp(pars[6]),
        Temp ~
          SSMtrend(
            degree = 2,
            Q = c(list(0), list(exp(pars[1])))) +
          SSMseasonal(
            sea.type = "dummy",
            period = 12,
            Q = exp(pars[2])) +
          SSMarima(
            ar = artransform(pars[3:4]),
            d = 0,
            Q = exp(pars[5])) + 
          SSMregression(
            ~ Kuroshio_scaled, Q = 0), # 外生変数
      data = ts_data)
    )
  }
  
  
  # 最適化その1。まずはNelder-Mead法を用いて暫定的なパラメータを推定
  fit_ssm_bef <- fitSSM(
    build_ssm,
    inits = c(-17,-30, 0.5, 0, -1,-5), # パラメータの初期値(任意)
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

#' 
#' # モデル関数の適用
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
list_M1 <- make_ssm_M1(SST_Kuroshio_ts)
fit_M1    <- list_M1[[1]]
result_M1 <- list_M1[[2]]

# 係数の95%信頼区間
res_M1 <- confint(result_M1, level = 0.95)


#' 
#' # 推定結果の確認
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
#モデル推定の収束の確認
fit_M1$optim.out$convergence

par_M1 <- fit_M1$optim.out$par #モデルの推定パラメーター
par_comp_M1 <- c(Q_trend  = exp(par_M1[1]), # 年トレンドの大きさ
                  Q_season = exp(par_M1[2]), # 季節トレンドの大きさ
                  AR1      = artransform(par_M1[3:4])[1], # 1次のARの大きさ
                  AR2      = artransform(par_M1[3:4])[2], # 2次のARの大きさ
                  Q_ar     = exp(par_M1[5]), # 短期変動の揺らぎ
                  H        = exp(par_M1[6])) # 観察誤差の大きさ
par_comp_M1

# 平滑化推定量
alpha_hat_M1 <- result_M1$alphahat
head(alpha_hat_M1)

#外生変数の効果の推定値
#全時点で固定された係数(Q = 0)だが機械的な丸め誤差が生じるため最初の値を用いる
beta_kuroshio_scaled <- alpha_hat_M1[,"Kuroshio_scaled"][1]
beta_kuroshio_scaled

ci_95_beta_M1 <- res_M1[c("Kuroshio_scaled")] %>% lapply(head, n = 1)
ci_95_beta_M1

#元スケールに変換(黒潮続流北限緯度が 1 度北上すると SST は beta推定値だけ変化)
sd_kuroshio <- sd(SST_Kuroshio_ts[,"Kuroshio"],na.rm=TRUE)
beta_kuroshio_per_deg <- beta_kuroshio_scaled /sd_kuroshio
beta_kuroshio_per_deg

level_M1 <- alpha_hat_M1[,"level"]
drift_M1 <- alpha_hat_M1[,"slope"]

level_M1_ts <- ts(level_M1, start = start(SST_ts), frequency = 12)
drift_M1_ts <- ts(drift_M1, start = start(SST_ts), frequency = 12)

# 馬場ら（2024)との比較のため, 2023年2月のドリフト成分の抽出
drift_M1_2023Feb <- window(drift_M1_ts, start=c(2023, 2), end=c(2023, 2))

# 年あたりに換算した2023年2月時点の瞬間的な昇温率:報告値その１
drift_M1_2023Feb_per_year <- drift_M1_2023Feb*12
print(drift_M1_2023Feb_per_year)

# 年あたりの平均的な昇温率:報告値その２
mean_drift_year_M1 <- mean(drift_M1_ts) * 12
print(mean_drift_year_M1)


# 成分別にプロット
M1_level_plot <- autoplot(result_M1$alphahat[,"level"]) +
  labs(y = "", x = "Time") +
  ggtitle("Level component")

M1_slope_plot <- autoplot(result_M1$alphahat[,"slope"]) +
  labs(y = "", x = "Time") +
  ggtitle("Drift component")

M1_season_plot <- autoplot(result_M1$alphahat[,"sea_dummy1"]) +
  labs(y = "", x = "Time") +
  ggtitle("Seasonal component")

M1_arima1_plot <- autoplot(result_M1$alphahat[,"arima1"]) +
  labs(y = "", x = "Time") +
  ggtitle("Auto-regression component")


M1_out_plot <- grid.arrange(M1_level_plot,
                            M1_slope_plot,
                            M1_season_plot,
                            M1_arima1_plot,
                            ncol = 1)

plot(M1_out_plot )

#' 
#' # モデル評価：残差のチェック
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
# 標準化残差
std_obs_resid_M1 <- rstandard(result_M1, type = "recursive")

# forecastパッケージのcheckredisuals関数で残差のチェック
# Ljung–Box検定: P > 0.05で残差に有意な自己相関なしと判断
checkresiduals(std_obs_resid_M1)

# 図示された残差（上：残差系列；左下：残差コレログラム；右下：残差のヒストグラム）をみて異常に突出した残差がないかなどを確認

#正規性の確認
# P > 0.05で正規分布と有意に異なっていないと判断
shapiro.test(std_obs_resid_M1)


#' 
#' # 水準変動の信頼区間の図示
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
level_M1_tidy <- cbind(
  data.frame(time=time(SST_ts),
             SST=as.numeric(SST_ts),
             level_M1=level_M1,
             level_kuroshio_scaled_M1=level_M1+beta_kuroshio_scaled*as.numeric(SST_Kuroshio_ts[,"Kuroshio_scaled"])
  ),
  as.data.frame(res_M1$level)
  ) %>%
  as_tibble() %>%
  rename(lwr_M1=lwr,upr_M1=upr)


level_M1_ggplot <- ggplot(data=level_M1_tidy,
                         aes(x=time,y=level_M1)) +
  labs(title="Level component",
       subtitle="Points indicate level plus effect of the exogenous variable",
       x="Year", y="SST") +
  geom_line(aes(y=level_M1), size = 1.2) +
  geom_point(aes(y=level_kuroshio_scaled_M1), size = 0.6) +
  geom_ribbon(aes(ymin = lwr_M1, ymax = upr_M1), alpha = 0.3)

ggsave("level_M1_plot.png",
       width=6, height=4,
       plot = level_M1_ggplot)

level_M1_ggplot

#' 
#' # ドリフト成分の図示
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
drift_M1_tidy <- cbind(
  data.frame(time=time(SST_ts),
             Temp=as.numeric(SST_ts),
             drift_M1=drift_M1),
  as.data.frame(res_M1$slope)
  ) %>%
  as_tibble() %>%
  rename(lwr_M1=lwr,upr_M1=upr)


annual_drift_lab_M1 <- paste0("average annual drift = ",round(mean_drift_year_M1,3))

drift_M1_ggplot <- ggplot(data=drift_M1_tidy,
                         aes(x=time,y=drift_M1)) +
  labs(title=paste0("Drift component: ",annual_drift_lab_M1),
       x="Year", y="Drift") +
  geom_line(aes(y=drift_M1), size = 1.2) +
  geom_ribbon(aes(ymin = lwr_M1, ymax = upr_M1), alpha = 0.3) +
  geom_hline(yintercept=0, linetype="dashed") 

ggsave("drift_M1_ggplot.png",
       width=6, height=4,
       plot = drift_M1_ggplot)

drift_M1_ggplot

#' 
#' # 水準のモデル間比較
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
level_M0_M1_tidy <- level_M0_tidy %>%
  mutate(level_M1= level_M1_tidy$level_M1,
         lwr_M1 = level_M1_tidy$lwr_M1,
         upr_M1 = level_M1_tidy$upr_M1)

level_2model_ggplot <- ggplot(data=level_M0_M1_tidy,
                         aes(x=time,y=level_M0)) +
  labs(title="Level component",
       subtitle="Model 0:red; Model 1: blue",
       x="Year", y="SST") +
  geom_line(aes(y=level_M0), size = 1.2, color= "#F8766D",linetype="dashed") +
  geom_line(aes(y=level_M1), size = 1.2, color= "#00BFC4") +
  geom_ribbon(aes(ymin = lwr_M0, ymax = upr_M0), fill = "#F8766D",alpha = 0.3) +
  geom_ribbon(aes(ymin = lwr_M1, ymax = upr_M1), fill = "#00BFC4",alpha = 0.3)

level_2model_ggplot

#' 
#' # ドリフト成分のモデル間比較
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------

drift_M0_M1_tidy <- drift_M0_tidy %>%
  mutate(drift_M1= drift_M1_tidy$drift_M1,
         lwr_M1 = drift_M1_tidy$lwr_M1,
         upr_M1 = drift_M1_tidy$upr_M1)

         
drift_2model_ggplot <- ggplot(data=drift_M0_M1_tidy,
                         aes(x=time,y=drift_M0)) +
  labs(title="Drift component",
       subtitle="Model 0:red; Model 1: blue",
       x="Year", y="Drift") +
  geom_line(aes(y=drift_M0), size = 1.2, color= "#F8766D",linetype="dashed") +
  geom_line(aes(y=drift_M1), size = 1.2, color= "#00BFC4") +
  geom_ribbon(aes(ymin = lwr_M0, ymax = upr_M0), fill = "#F8766D",alpha = 0.3) +
  geom_ribbon(aes(ymin = lwr_M1, ymax = upr_M1), fill = "#00BFC4",alpha = 0.3) +
  geom_hline(yintercept=0, linetype="dashed") 

drift_2model_ggplot 


#' 
#' # AICによるモデルの比較：M0 vs. M1
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------

# AIC算出関数の定義
calc_AIC <- function(fit_ssm){
  ll <- logLik(fit_ssm$model)
  k <- length(fit_ssm$optim.out$par)
  AIC <-  -2 * as.numeric(ll) + 2 * k
  
  return(AIC)
}

AIC_M0 <- calc_AIC(fit_M0)
AIC_M1 <- calc_AIC(fit_M1)

AIC_M0
AIC_M1


#' 
#' # 交差検証によるモデルの比較：M0 vs. M1
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
SST_Kuroshio_ts_train <- window(SST_Kuroshio_ts, end = c(2010, 12))
SST_Kuroshio_ts_test  <- window(SST_Kuroshio_ts, start = c(2011, 1))

list_M0_train <- make_ssm_M0(SST_Kuroshio_ts_train)
list_M1_train <- make_ssm_M1(SST_Kuroshio_ts_train)

pars_M0 <- list_M0_train[[1]]$optim.out$par
pars_M1 <- list_M1_train[[1]]$optim.out$par


pred_M0 <- predict(
  list_M0_train[[2]]$model, 
  newdata = SSModel(
    H = exp(pars_M0[6]),
    rep(NA, nrow(SST_Kuroshio_ts_test)) ~
      SSMtrend(degree = 2,
               Q = c(list(0), list(exp(pars_M0[1])))) +
      SSMseasonal(sea.type = "dummy",
                  period = 12,
                  Q = exp(pars_M0[2])) +
      SSMarima(ar = artransform(pars_M0[3:4]),
               d = 0,
               Q = exp(pars_M0[5])),
    data = SST_Kuroshio_ts_test
  )
)


pred_M1 <- predict(
  list_M1_train[[2]]$model, 
  newdata = SSModel(
    H = exp(pars_M1[6]),
    rep(NA, nrow(SST_Kuroshio_ts_test)) ~
      SSMtrend(degree = 2,
               Q = c(list(0), list(exp(pars_M1[1])))) +
      SSMseasonal(sea.type = "dummy",
                  period = 12,
                  Q = exp(pars_M1[2])) +
      SSMarima(ar = artransform(pars_M1[3:4]),
               d = 0,
               Q = exp(pars_M1[5])) + 
        SSMregression(
          ~ Kuroshio_scaled, Q = 0
        ),
    data = SST_Kuroshio_ts_test
  )
)


CV_out_M0 <- accuracy(pred_M0, SST_Kuroshio_ts_test[, "Temp"])
CV_out_M1 <- accuracy(pred_M1, SST_Kuroshio_ts_test[, "Temp"])

CV_out_M0
CV_out_M1

#' # Rスクリプトの出力
#' 
## ----message = FALSE, warning = FALSE, echo = TRUE----------------------------
knitr::purl("SST_ts_analyses.Rmd", 
            output ="SST_ts_analyses.r",
            documentation = 2)

