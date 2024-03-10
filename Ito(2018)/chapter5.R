## Machine-Learning-with-R/chapter5.R
## 2024-03-07
## Nakano Keisuke

## 準備
library(tidyverse)
library(plotly)


# リスト 5-1-(1)
## %matplotlib inline
## import numpy as np
## import matplotlib.pyplot as plt

## # データ生成 ----------
## np.random.seed(seed=1)          # 乱数を固定する
set.seed(1)
## X_min, X_max = 4, 30            # X の下限と上限（表示用）
X_min <- 4; X_max <- 30
## N = 16                          # データの個数
N <- 16
## X = 5 + 25 * np.random.rand(N)  # X の生成
X <- 5 + 25 * runif(N)
## prm = [170, 108, 0.2]           # データ生成のパラメータ
prm <- c(170, 108, 0.2)
## T = prm[0] - prm[1] * np.exp(-prm[2] * X) \
## + 4 * np.random.randn(N)    # (A) 目標データの生成
t <- prm[1] - prm[2] * exp(-prm[3] * X) + 4 * rnorm(N) # Tは予約語であるためtとする．
## np.savez(                       # (B) データの保存
##   "ch5_data.npz",
##   X=X, T=T, X_min=X_min, X_max=X_max, N=N,
## )
write.csv(tibble(X = X, t = t),
          file = 'ch5_data.csv')

# リスト 5-1-(2)
## print(X)
print(X)


# リスト 5-1-(3)
## print(np.round(X, 2))
print(round(X, 2))


# リスト 5-1-(4)
## print(np.round(T, 2))
print(round(t, 2))


# リスト 5-1-(5)
## # データ表示 ----------
## plt.figure(figsize=(4, 4))
## plt.plot(
##   X,                        # 入力データ
##   T,                        # 目標データ
##   "cornflowerblue",         # マーカーを水色にする
##   marker="o",               # マーカーの形をoにする
##   linestyle="None",         # マーカーを線で結ばない
##   markeredgecolor="black",  # マーカーの輪郭を黒にする
## )
## plt.xlim(X_min, X_max)        # x軸の範囲を指定
## plt.grid()                    # グリッドを表示する
## plt.show()
ggplot() +
  geom_point(data    = tibble(a = X, b = t),
             mapping = aes(x = a, y = b),
             color = 'cornflowerblue') +
  labs(x = element_blank(), y = element_blank()) +
  xlim(5, 30) + ylim(140, 180) +
  coord_fixed(ratio = 25 / 40)


# リスト 5-1-(6)
## # 平均二乗誤差(MSE)関数 ----------
## def mse_line(x, t, w):
##   y = w[0] * x + w[1]          # 式5-4、yを求める
##   mse = np.mean((y - t) ** 2)  # 式5-3、平均二乗誤差
##   return mse
mse_line <- function(x, t, w) {
  y <- w[1] * x + w[2]
  mse <- mean((y - t)^2)
  return(mse)
}

## # 各w0、w1で平均二乗誤差(MSE)を計算 ----------
## w0_n, w1_n = 100, 100                   # グラフ表示の解像度
w0_n <- 100; w1_n <- 100
## w0_min, w0_max = -25, 25                # w0の計算範囲
w0_min <- -25; w0_max <- 25
## w1_min, w1_max = 120, 170               # w1の計算範囲
w1_min <- 120; w1_max <- 170
## w0 = np.linspace(w0_min, w0_max, w0_n)  # w0を準備
w0 <- seq(w0_min, w0_max, length.out = w0_n)
## w1 = np.linspace(w1_min, w1_max, w1_n)  # w1を準備
w1 <- seq(w1_min, w1_max, length.out = w1_n)
## J = np.zeros((w1_n, w0_n))              # MSEを入れる配列Jを準備
J <- array(0, dim = c(w1_n, w0_n))
## # グリッド状の点(w0, w1)に対してJを計算
## for i0 in range(w0_n):
##   for i1 in range(w1_n):
##     w = np.array([w0[i0], w1[i1]])
##     J[i1, i0] = mse_line(X, T, w)
for (i0 in 1:w0_n) {
  for (i1 in 1:w1_n) {
    w <- array(c(w0[i0], w1[i1]), dim = c(1, 2))
    J[i1, i0] <- mse_line(X, t, w)
  }
}
## ww0, ww1 = np.meshgrid(w0, w1)          # グリッド座標の作成
ww0 <- as.array(matrix(w0, nrow = w0_n, ncol = w1_n, byrow = TRUE))
ww1 <- array(w1, dim = c(w0_n, w1_n))

## # グラフ描画 ----------
## plt.figure(figsize=(9.5, 4))
## plt.subplots_adjust(wspace=0.5)
## # サーフェス表示
## ax = plt.subplot(1, 2, 1, projection="3d")
## ax.plot_surface(
##   ww0, ww1, J,
##   rstride=10, cstride=10, alpha=0.3, color="blue", edgecolor="black",
## )
## ax.set_xticks([-20, 0, 20])     # x軸の目盛り指定
## ax.set_yticks([120, 140, 160])  # y軸の目盛り指定
## ax.view_init(20, -60)           # グラフの向きの指定
df <- tibble(w0 = rep(w0, each = w0_n),
             w1 = rep(w1, times = w1_n),
             J = rep(NA, times = w0_n * w1_n))
n <- 1
for (i in 1:w0_n) {
  for (j in 1:w1_n) {
    df$J[n] <- J[i, j]
    n <- n + 1
  }
}
plot_ly() |>
  add_trace(
    data = df,
    x = df$w0,
    y = df$w1,
    z = df$J,
    alpha = 0.7,
    size = 3)
# 追記
persp(w0, w1, J, theta = -60, phi = 10, lwd = 0.5)
## # 等高線表示
## plt.subplot(1, 2, 2)
## cont = plt.contour(
##   ww0, ww1, J, colors="black",
##   levels=[100, 1000, 10000, 100000],  # 描く等高線の値を指定
## )
## cont.clabel(fmt="%d", fontsize=8)
## plt.grid()
## plt.show()
df |>
  ggplot() +
  geom_contour(aes(x = w0, y = w1, z = J),
               color = 'black') +
  xlim(-25, 25) + ylim(120, 170) +
  coord_flip() +
  theme(aspect.ratio = 1)


# リスト 5-1-(7)
## # 平均二乗誤差(MSE)の勾配 ----------
## def dmse_line(x, t, w):
##   y = w[0] * x + w[1]
##   d_w0 = 2 * np.mean((y - t) * x)  # 式5-9
##   d_w1 = 2 * np.mean(y - t)        # 式5-10
##   return d_w0, d_w1
dmse_line <- function(x, t, w) {
  y <- w[1] * x + w[2]
  d_w0 <- 2 * mean((y - t) * x)
  d_w1 <- 2 * mean(y - t)
  return(c(d_w0, d_w1))
}


# リスト 5-1-(8)
## w = np.array([10, 165])
w <- array(c(10, 165), dim = c(1, 2))
## d_w = dmse_line(X, T, w)
d_w <- dmse_line(X, t, w)
## print(np.round(d_w, 2))
print(round(d_w, 2))


# リスト 5-1-(9)
## # 勾配法 ----------
## def fit_line_num(x, t, w_init):
##   # パラメータ
##   alpha = 0.001     # 学習率
##   tau_max = 100000  # 繰り返しの最大数
##   eps = 0.1         # 繰り返し計算を終了するための閾値
##   # 勾配法
##   w = np.zeros((tau_max + 1, 2))  # 変化するwの履歴を入れる配列
##   w[0, :] = w_init                # wの初期値をセット
##   for tau in range(tau_max):
##     dmse = dmse_line(x, t, w[tau, :])
##     w[tau + 1, 0] = w[tau, 0] - alpha * dmse[0]  # 式5-6
##     w[tau + 1, 1] = w[tau, 1] - alpha * dmse[1]  # 式5-7
##     if max(np.absolute(dmse)) < eps:  # 終了判定
##        break  # tau のループから抜ける
##  w_final = w[tau + 1, :]   # 最終的に得られたw
##  w_hist = w[: tau + 2, :]  # wの履歴で更新した分を抜き出す
##  return w_final, dmse, w_hist
fit_line_num <- function(x, t, w_init) {
  alpha <- 0.001
  tau_max <- 100000
  eps <- 0.1
  
  w <- array(0, dim = c(tau_max + 1, 2))
  w[1,] <- w_init
  for (tau in 1:tau_max) {
    dmse <- dmse_line(x, t, w[tau,])
    w[tau + 1, 1] <- w[tau, 1] - alpha * dmse[1]
    w[tau + 1, 2] <- w[tau, 2] - alpha * dmse[2]
    if (max(abs(dmse)) < eps) break
  }
  w_final <- w[tau + 1,]
  w_hist <- w[1:(tau + 2),]
  return(list(w_final = w_final, dmse = dmse, w_hist = w_hist))
}

## # メイン ----------
## # 勾配法でwを計算
## w_init = np.array([10.0, 165.0])               # wの初期値
w_init <- array(c(10, 165), dim = c(1, 2))
## w, dmse, w_history = fit_line_num(X, T, w_init)  # wを計算
res <- fit_line_num(X, t, w_init)
w <- res$w_final; dmse <- res$dmse; w_history <- res$w_hist
## mse = mse_line(X, T, w)                        # MSEを計算
mse <- mse_line(X, t, w)
## # 結果表示
## print(f"繰り返し回数 {w_history.shape[0]-1}")
cat('繰り返し数', nrow(w_history))
## print(f"w0 = {w[0]:.6f}, w1 = {w[1]:.6f}")
cat('w0 =', round(w[1],6), 'w1 =', round(w[2], 6))
## print(f"dMSE = [{dmse[0]:.6f}, {dmse[1]:.6f}]")
cat('dMSE =', '[', dmse[1], ',', dmse[2], ']')
## print(f"MSE = {mse:.6f}")
cat('MSE =', round(mse, 6))
## print(f"SD = {np.sqrt(mse):.6f} cm")
cat('SD =', sqrt(mse) |> round(6), 'cm')

## # グラフ描画 ----------
## plt.figure(ﬁgsize=(4, 4))
## # 等高線表示
## cont = plt.contour(
##   ww0, ww1, J,     # リスト5-1-(6)で作成済
##   colors="black", levels=[100, 1000, 10000, 100000],
## )
## cont.clabel(fmt="%1.0f", fontsize=8)
## # 等高線の上に、過去のすべてのwをプロット
## plt.plot(  
##   w_history[:, 0], w_history[:, 1], "gray",
##   marker=".",                        # マーカーの形
##   markersize=10,                     # マーカーの大きさ
##   markeredgecolor="cornflowerblue",  # マーカーの輪郭の色
## )
## plt.grid()
## plt.show()
ggplot() +
  geom_contour(data    = df,
               mapping = aes(w0, w1, z = J),
               color = 'black') +
  geom_point(data = tibble(a = w_history[,1], b = w_history[,2]),
             mapping = aes(x = a, y = b),
             color = 'cornflowerblue') +
  xlim(-25, 25) + ylim(120, 170) +
  coord_flip() +
  theme(aspect.ratio = 1)


# リスト 5-1-(10)
## # 線の表示 ----------
## def show_line(w):
##   x = np.linspace(X_min, X_max, 100)
##   y = w[0] * x + w[1]
##   plt.plot(x, y, "gray", linewidth=4)
show_line <- function(w) {
  x = seq(X_min, X_max, length.out = 100)
  y <- w[1] * x + w[2]
  p <- ggplot() +
    geom_line(data    = tibble(a = x, b = y),
              mapping = aes(x = a, y = b),
              color   = 'gray')
  return(p)
}

## # メイン ----------
## # グラフ描画
## plt.figure(figsize=(4, 4))
## show_line(w)  # w はリスト5-1-(9)で計算済
## plt.plot(
##   X, T, "cornflowerblue",
##   marker="o", linestyle="None", markeredgecolor="black",
## )
## plt.xlim(X_min, X_max)
## plt.grid()
## plt.show()
show_line(w) +
  geom_point(data    = tibble(a = X, b = t),
             mapping = aes(x = a, y = b),
             color   = 'cornflowerblue') +
  xlim(5, 30) + ylim(140, 180) +
  coord_fixed(ratio = 25 / 40) +
  labs(x = element_blank(), y = element_blank())


# リスト 5-1-(11)
## # 解析解 ----------
## def fit_line(x, t):
##   mx = np.mean(x)        # <x>
##   mt = np.mean(t)        # <t>
##   mtx = np.mean(t * x)   # <tx>
##   mxx = np.mean(x * x)   # <x^2>
##   w0 = (mtx - mt * mx) / (mxx - mx ** 2)  # 式5-20
##   w1 = mt - w0 * mx                       # 式5-21
##   w = np.array([w0, w1])
##   return w
fit_line <- function(x, t) {
  mx <- mean(x)
  mt <- mean(t)
  mtx <- mean(t * x)
  mxx <- mean(x * x)
  w0 <- (mtx - mt * mx) / (mxx - mx^2)
  w1 <- mt - w0 * mx
  w <- array(c(w0, w1), dim = c(1, 2))
  return(w)
}

## # メイン ----------
## w = fit_line(X, T)       # 解析解でwを計算
w <- fit_line(X, t)
## mse = mse_line(X, T, w)  # MSEを計算
mse <- mse_line(X, t, w)
## # 結果表示
## print(f"w0 = {w[0]:.2f}, w1 = {w[1]:.2f}")
cat('w0 =', round(w[1], 2), 'w1 =', round(w[2], 2))
## print(f"MSE = {mse:.2f}")
cat('MSE =', round(mse, 2))
## print(f"SD = {np.sqrt(mse):.2f} cm")
cat('SD =', sqrt(mse) |> round(2), 'cm')

## # グラフ描画 ----------
## plt.figure(figsize=(4, 4))
## show_line(w)
## plt.plot(
##   X, T, "cornflowerblue",
##   marker="o", linestyle="None", markeredgecolor="black",
## )
## plt.xlim(X_min, X_max)
## plt.grid()
## plt.show()
show_line(w) +
  geom_point(data    = tibble(a = X, b = t),
             mapping = aes(x = a, y = b),
             color   = 'cornflowerblue') +
  xlim(5, 30) + ylim(140, 180) +
  coord_fixed(ratio = 25 / 40) +
  labs(x = element_blank(), y = element_blank())


## %reset
rm(list = ls(all.names = TRUE))


# リスト 5-2-(1)
## %matplotlib inline
## import numpy as np
## import matplotlib.pyplot as plt

## # データのロード ----------
## data = np.load("ch5_data.npz")
data <- read.csv('ch5_data.csv')
## X0 = data["X"]  # これまでのXをX0とする
X0 <- data$X
## N = data["N"]
N <- 16
## T = data["T"]
t <- data$t

## # 2次元データ生成 ----------
## np.random.seed(seed=1)  # 乱数を固定
set.seed(1)
## X1 = 23 * (T / 100) ** 2 + 2 * np.random.randn(N)  # X1を生成
X1 <- 23 * (t / 100)^2 + 2 * rnorm(N)
## X0_min, X0_max = 5, 30   # X0の下限と上限（表示用）
X0_min <- 5; X0_max <- 30
## X1_min, X1_max = 40, 75  # X1の下限と上限（表示用）
X1_min <- 40; X1_max <- 75


# リスト 5-2-(2)
## print(np.round(X0, 2))
print(round(X0, 2))
## print(np.round(X1, 2))
print(round(X1, 2))
## print(np.round(T, 2))
print(round(t, 2))


# リスト 5-2-(3)
## # 2次元データの表示 ----------
## def show_data2d(ax, x0, x1, t):  # axは3dグラフ描画のため
##   for i in range(len(x0)):
##     ax.plot(  # データ点の下の直線の描画
##       [x0[i], x0[i]],       # 直線の両端のx座標
##       [x1[i], x1[i]],       # 直線の両端のy座標
##       [120, t[i]],          # 直線の両端のz座標
##       color="gray",
##     )
##   ax.plot(      # データ点の描画
##     x0,                       # x座標
##     x1,                       # y座標
##     t,                        # z座標
##     "cornflowerblue",         # 色
##     marker="o",               # マーカーの形状
##     linestyle="None",         # 点をつなげる線は描かない
##     markeredgecolor="black",  # マーカーの輪郭の色
##     markersize=6,             # マーカーのサイズ
##     markeredgewidth=0.5,      # マーカーの輪郭線の太さ
##   )
##   ax.view_init(elev=35, azim=-75)  # グラフの向きの指定

## # メイン ----------
## plt.figure(figsize=(6, 5))
## ax = plt.subplot(projection="3d")
## show_data2d(ax, X0, X1, T)
## plt.show()
scatterplot3d::scatterplot3d(x = X0, y = X1, z = t, type = 'h')

# リスト 5-2-(4)
## # 面の表示 ----------
## def show_plane(ax, w):
##   # 表示データの計算
##   x0_n, x1_n = 5, 5
##   x0 = np.linspace(X0_min, X0_max, x0_n)
##   x1 = np.linspace(X1_min, X1_max, x1_n)
##   xx0, xx1 = np.meshgrid(x0, x1)  # グリッド座標の作成
##   y = w[0] * xx0 + w[1] * xx1 + w[2]  # (A) 式5-28
##   # サーフェス表示
##   ax.plot_surface(
##     xx0, xx1, y,
##     rstride=1, cstride=1, alpha=0.3, color="blue", edgecolor="black",
##   )
show_plane <- function(w) {
  x0_n <- 5; x1_n <- 5
  x0 <- seq(X0_min, X0_max, length.out = x0_n)
  x1 <- seq(X1_min, X1_max, length.out = x1_n)
  xx0 <- as.array(matrix(x0, nrow = x0_n, ncol = x1_n, byrow = TRUE))
  xx1 <- array(x1, dim = c(x0_n, x1_n))
  n <- 1
  df <- data.frame(array(NA, dim = c(x0_n * x1_n, 3))) |>
    rename(x0 = 'X1', x1 = 'X2', y = 'X3')
  for (i in 1:x0_n) {
    for (j in 1:x1_n) {
      df[n, 1] <- xx0[i, j]
      df[n, 2] <- xx1[i, j]
      df[n, 3] <- w[1] * xx0[i, j] + w[2] * xx1[i, j] + w[3]
      n <- n + 1
    }
  }
  plot3d(df$x0, df$x1, df$y, type = 's')
}

## # 面の平均二乗誤差(MSE)関数 ----------
## def mse_plane(x0, x1, t, w):
##   y = w[0] * x0 + w[1] * x1 + w[2]  # (A) 式5-28
##   mse = np.mean((y - t) ** 2)
##   return mse
mse_plane <- function(x0, x1, t, w) {
  y <- w[1] * x0 + w[2] * x1 + w[3]
  mse <- mean((y - t)^2)
  return(mse)
}

## # メイン ----------
## w = np.array([1.5, 1, 90])
w <- array(c(1.5, 1, 90), dim = c(1, 3))
## mse = mse_plane(X0, X1, T, w)  # MSEを計算
mse <- mse_plane(X0, X1, t, w)
## # 結果表示
## print(f"SD = {np.sqrt(mse):.2f} cm")
cat('SD =', sqrt(mse) |> round(2), 'cm')

## # グラフ描画 ----------
## plt.figure(figsize=(6, 5))
## ax = plt.subplot(projection="3d")
## show_plane(ax, w)
## show_data2d(ax, X0, X1, T)
## plt.show()
show_plane(w) # 面にする関数が見つからなかったので一旦点で．


# リスト 5-2-(5)
## # 解析解 ----------
## def fit_plane(x0, x1, t):  
##   c_tx0 = np.mean(t * x0) - np.mean(t) * np.mean(x0)     # cov(t, x0)
##   c_tx1 = np.mean(t * x1) - np.mean(t) * np.mean(x1)     # cov(t, x1)
##   c_x0x1 = np.mean(x0 * x1) - np.mean(x0) * np.mean(x1)  # cov(x0, x1)
##   v_x0 = np.var(x0)                                      # var(x0)
##   v_x1 = np.var(x1)                                      # var(x1)
##   # 式5-34
##   w0 = (c_tx1 * c_x0x1 - v_x1 * c_tx0) / (c_x0x1 ** 2 - v_x0 * v_x1)
##   # 式5-35
##   w1 = (c_tx0 * c_x0x1 - v_x0 * c_tx1) / (c_x0x1 ** 2 - v_x0 * v_x1)
##   # 式5-36
##   w2 = -w0 * np.mean(x0) - w1 * np.mean(x1) + np.mean(t)
##   w = np.array([w0, w1, w2])
##   return w
fit_plane <- function(x0, x1, t) {
  c_tx0 <- mean(t * x0) - mean(t) * mean(x0)
  c_tx1 <- mean(t * x1) - mean(t) * mean(x1)
  c_x0x1 <- mean(x0 * x1) - mean(x0) * mean(x1)
  v_x0 <- var(x0)
  v_x1 <- var(x1)
  w0 <- (c_tx1 * c_x0x1 - v_x1 * c_tx0) / (c_x0x1^2 - v_x0 * v_x1)
  w1 <- (c_tx0 * c_x0x1 - v_x0 * c_tx1) / (c_x0x1^2 - v_x0 * v_x1)
  w2 <- -w0 * mean(x0) - w1 * mean(x1) + mean(t)
  w <- array(c(w0, w1, w2), dim = c(1, 3))
  return(w)
}

## # メイン ----------
## w = fit_plane(X0, X1, T)       # wを計算
w <- fit_plane(X0, X1, t)
## mse = mse_plane(X0, X1, T, w)  # MSEを計算
mse <- mse_plane(X0, X1, t, w)
## # 結果表示
## print(f"w0 = {w[0]:.2f}, w1 = {w[1]:.2f}, w2 = {w[2]:.2f}")
cat('w0 =', round(w[1], 2), 'w1 =', round(w[2], 2), 'w2 =', round(w[3], 2))
## print(f"SD = {np.sqrt(mse):.2f} cm")
cat('SD =', sqrt(mse) |> round(2), 'cm')

## # グラフ描画 ----------
## plt.figure(figsize=(6, 5))
## ax = plt.subplot(projection="3d")
## show_plane(ax, w)
## show_data2d(ax, X0, X1, T)
## plt.show()
planes3d(w[1], w[2], -1, w[3])


## %reset
rm(list = ls(all.names = TRUE))


# リスト 5-3-(1)
## %matplotlib inline
## import numpy as np
## import matplotlib.pyplot as plt
NULL

## # データのロード ----------
## data = np.load("ch5_data.npz")
data <- read.csv('ch5_data.csv')
## X = data["X"]
X <- data$X
## X_min = 0
X_min <- 0
## X_max = data["X_max"]
X_max <- 30
## N = data["N"]
N <- 16
## T = data["T"]
t <- data$t


# リスト 5-3-(2)
## # ガウス関数 ----------
## def gauss(x, mu, s):
##   y = np.exp(-((x - mu) ** 2) / (2 * s ** 2))  # 式5-64
##   return y
gauss <- function(x, mu, s) {
  y <- exp(-((x - mu)^2) / (2 * s^2))
  return(y)
}


# リスト 5-3-(3)
## # メイン ----------
## M = 4                                # ガウス関数の数
M <- 4
## mu = np.linspace(5, 30, M)           # 平均パラメータ
mu <- seq(5, 30, length.out = M)
## s = mu[1] - mu[0]                    # (A) 標準偏差パラメータ
s <- mu[2] - mu[1]
## xb = np.linspace(X_min, X_max, 100)
xb <- seq(X_min, X_max, length.out = 100)
## y = np.zeros((M, 100))  # M個のガウス関数の値を入れるyを準備
y <- array(0, dim = c(100, M)) |> # 便宜上行と列を入れ替える．
  data.frame() |>
  rename(mu5 = 'X1',
         mu13.3 = 'X2',
         mu21.7 = 'X3',
         mu30 = 'X4')
## for j in range(M):
##   y[j, :] = gauss(xb, mu[j], s)    # ガウス関数
for (j in 1:M) {
  y[, j] <- gauss(xb, mu[j], s)
}

## # グラフ描画 ----------
## plt.figure(figsize=(4, 4))
## for j in range(M):
##   plt.plot(xb, y[j, :], "gray", linewidth=3)
##   plt.xlim(X_min, X_max)
##   plt.ylim(0, 1.2)
##   plt.grid()
##   plt.show()
y <- y |>
  pivot_longer(cols = mu5:mu30,
               names_to = 'mu',
               values_to = 'gauss') |>
  mutate(x = rep(xb, each = 4))
y |> ggplot() +
  geom_line(aes(x = x, y = gauss,
                color = mu)) +
  labs(x = element_blank(), y = element_blank()) +
  xlim(0, 30) + ylim(0, 1) +
  coord_fixed(ratio = 30 / 1) +
  theme(legend.position = 'none')


# リスト 5-3-(4)
## # 線形基底関数モデル ----------
## def gauss_func(w, x):
##   m = len(w) - 1        # ガウス関数の数
##   mu = np.linspace(5, 30, m)
##   s = mu[1] - mu[0]
##   # xと同じサイズで要素が0のndarray型を作成
##   y = np.zeros_like(x)
##   # ここでは式5-66ではなく式5-65で実装
##   for j in range(m):
##     y = y + w[j] * gauss(x, mu[j], s)
##   y = y + w[m]  # phiを掛けないパラメータを最後に加える
##   return y
gauss_func <- function(w, x) {
  m <- length(w) - 1
  mu <- seq(5, 30, length.out = m)
  s = mu[2] - mu[1]
  y <- array(0, dim = c(1, length(x)))
  for (j in 1:m) {
    y <- y + w[j] * gauss(x, mu[j], s)
  }
  y <- y + w[m]
  return(y)
}


# リスト 5-3-(5)
## # 線形基底関数モデルの平均二乗誤差(MSE) ----------
## def mse_gauss_func(x, t, w):
##   y = gauss_func(w, x)
##   mse = np.mean((y - t) ** 2)
##   return mse
mse_gauss_func <- function(x, t, w) {
  y <- gauss_func(w, x)
  mse <- mean((y - t)^2)
  return(mse)
}


# リスト 5-3-(6)
## # 線形基底関数モデルの厳密解 ----------
## def fit_gauss_func(x, t, m):
##   mu = np.linspace(5, 30, m)
##   s = mu[1] - mu[0]
##   n = x.shape[0]
##   # 式5-69 の計画行列phiを作成
##   phi = np.ones((n, m + 1))  # (A) 要素が1のn x (m+1)行列
##   for j in range(m):         # (B) 0～m-1列に値を割り振る
##     phi[:, j] = gauss(x, mu[j], s)
##   # 式5-68 で厳密解のwを計算
##   w = np.linalg.inv(phi.T @ phi) @ phi.T @ t
##   return w
fit_gauss_func <- function(x, t, m) {
  mu <- seq(5, 30, length.out = m)
  s <- mu[2] - mu[1]
  n <- length(x)
  phi <- array(1, dim = c(n, m + 1))
  for (j in 1:m) {
    phi[, j] <- gauss(x, mu[j], s)
  }
  w <- solve(t(phi) %*% phi) %*% t(phi) %*% t
  return(w)
}


# リスト 5-3-(7)
## # ガウス基底関数表示 ----------
## def show_gauss_func(w):
##   x = np.linspace(X_min, X_max, 100)
##   y = gauss_func(w, x)
##   plt.plot(x, y, "gray", linewidth=4)
show_gauss_func <- function(w) {
  x <- seq(X_min, X_max, length.out = 100)
  y <- gauss_func(w, x)
  p <- tibble(a = x, b = y |> as.vector()) |>
    ggplot() +
    geom_line(aes(x = a, y = b),
              color = 'gray')
  return(p)
}

## # メイン ----------
## M = 4                          # ガウス関数の数
M <- 4
## w = fit_gauss_func(X, T, M)    # wを計算
w <- fit_gauss_func(X, t, M)
## mse = mse_gauss_func(X, T, w)  # MSEを計算
mse <- mse_gauss_func(X, t, w)
## # 結果表示
## print("w = ", np.round(w, 2))
cat('w =', round(w, 2))
## print(f"SD = {np.sqrt(mse):.2f} cm")
cat('SD =', sqrt(mse) |> round(2), 'cm')

## # グラフ描画 ----------
## plt.figure(figsize=(4, 4))
## show_gauss_func(w)
## plt.plot(
##   X, T, "cornflowerblue", 
##   marker="o", linestyle="None", markeredgecolor="black",
## )
## plt.xlim(X_min, X_max)
## plt.grid()
## plt.show()
show_gauss_func(w) +
  geom_point(data    = tibble(a = X, b = t),
             mapping = aes(x = a, y = b),
             color   = 'cornflowerblue')

# list 5-2-5, 3dplot
# 途中．PytyhonとRの行列計算に違い．