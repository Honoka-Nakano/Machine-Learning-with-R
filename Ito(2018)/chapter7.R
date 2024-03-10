## Machine-Learning-with-R/chapter7.R
## 2024-03-10
## Nakano Keisuke

## 準備
library(tidyverse)

# -- リスト 7-1-(1)
## import numpy as np
NULL

## # データ生成 ----------
## np.random.seed(seed=1)                # 乱数を固定
set.seed(1)
## N = 200                               # データの数
N <- 200
## K = 3                                 # 分布の数
K <- 3
## T = np.zeros((N, 3), dtype=np.uint8)  # 空のTを準備
t <- array(0, dim = c(N, 3)) # Tは予約語なのでtを使用
## X = np.zeros((N, 2))                  # 空のXを準備
X <- array(0, dim = c(N, 2))
## X0_min, X0_max = -3, 3                # X0の範囲、表示用
X0_min <- -3; X0_max <- 3
## X1_min, X1_max = -3, 3                # X1の範囲、表示用
X1_min <- -3; X1_max <- 3
## prm_mu = np.array([[-0.5, -0.5], [0.5, 1.0], [1, -0.5]])   # 分布の中心
prm_mu <- array(c(-0.5, 0.5, 1, -0.5, 1.0, -0.5), dim = c(3, 2))
## prm_sig = np.array([[0.7, 0.7], [0.8, 0.3], [0.3, 0.8]])   # 分布の分散
prm_sig <- array(c(0.7, 0.8, 0.3, 0.7, 0.3, 0.8), dim = c(3, 2))
## prm_pi = np.array([0.4, 0.8, 1])       # 各分布への割合を決めるパラメータ
prm_pi <- array(c(0.4, 0.8, 1), dim = c(1, 3))
## for n in range(N):
##   r = np.random.rand()
##   # 3クラス用の目標データTを作成
##   for k in range(K):
##     if r < prm_pi[k]:
##       T[n, k] = 1
##       break
##   # Tに対して入力データXを作成
##   for k in range(2):
##     X[n, k] =  \
##       np.random.randn() * prm_sig[T[n, :] == 1, k] \
##       + prm_mu[T[n, :] == 1, k]
for (n in 1:N) {
  r <- runif(1)
  for (k in 1:K) {
    if (r < prm_pi[k]) {
      t[n, k] <- 1
      break
    }
  }
  for (k in c(1, 2)) {
    X[n, k] <- rnorm(1) * prm_sig[t[n,] == 1, k] + prm_mu[t[n,] == 1, k]
  }
}


# リスト 7-1-(2)
## # 訓練データとテストデータに分割 ----------
## TrainingRatio = 0.5
TrainingRatio <- 0.5
## N_training = int(N * TrainingRatio)
N_training <- as.integer(N * TrainingRatio)
## X_train = X[:N_training, :]
X_train <- X[1:N_training,]
## X_test = X[N_training:, :]
X_test <- X[(N_training + 1):nrow(X),]
## T_train = T[:N_training, :]
T_train <- t[1:N_training,]
## T_test = T[N_training:, :]
T_test <- t[(N_training + 1):nrow(t),]

## # データの保存 ----------
## np.savez(
##   "ch7_data.npz",
##   X_train=X_train, T_train=T_train, X_test=X_test, T_test=T_test,
##   X0_min=X0_min, X0_max=X0_max, X1_min=X1_min, X1_max=X1_max,
## )
NULL


# リスト 7-1-(3)
## %matplotlib inline
## import matplotlib.pyplot as plt
NULL

## # データ表示 ----------
## def show_data(x, t):
##   K = t.shape[1]  # tの列数からクラス数を取得
##   col = ["gray", "white", "black"]
##   for k in range(K):
##     plt.plot(
##       x[t[:, k] == 1, 0], x[t[:, k] == 1, 1], col[k],
##       marker="o", linestyle="None",
##       markeredgecolor="black", alpha=0.8,
##     )
##   plt.xlim(X0_min, X0_max)
##   plt.ylim(X1_min, X1_max)
show_data <- function(x, t) {
  K <- ncol(t)
  col <- c('gray', 'white', 'black')
  df <- array(NA, dim = c(nrow(t), 3)) |>
                data.frame() |>
    rename(x0 = 'X1', x1 = 'X2', class = 'X3')
  for (i in 1:nrow(df)) {
    df[i, 1] <- x[i, 1]
    df[i, 2] <- x[i, 2]
    for (k in 1:K) {
      if (t[i, k] == 1) {
        df[i, 3] <- k
        break
      }
    }
  }
  p <- df |>
    ggplot() +
    geom_point(aes(x = x0, y = x1,
               color = as.factor(class),
               shape = as.factor(class))) +
    labs(x = element_blank(), y = element_blank())
  return(p)
}

## # メイン ----------
## plt.figure(figsize=(8, 3.7))
## # 訓練データ表示
## plt.subplot(1, 2, 1)
## show_data(X_train, T_train)
## plt.title("Training Data")
## plt.grid()
show_data(X_train, T_train) +
  xlim(X0_min, X0_max) + ylim(X1_min, X1_max) +
  coord_fixed(ratio = (X0_max - X0_min) / (X1_max - X1_min)) +
  scale_color_discrete(name = 'Class') +
  scale_shape_discrete(name = 'Class')
  
## # テストデータ表示
## plt.subplot(1, 2, 2)
## show_data(X_test, T_test)
## plt.title("Test Data")
## plt.grid()
## plt.show()
show_data(X_test, T_test) +
  xlim(X0_min, X0_max) + ylim(X1_min, X1_max) +
  coord_fixed(ratio = (X0_max - X0_min) / (X1_max - X1_min)) +
  scale_color_discrete(name = 'Class') +
  scale_shape_discrete(name = 'Class')


# リスト 7-1-(4)
## # シグモイド関数 ----------
## def sigmoid(a):
##   y = 1 / (1 + np.exp(-a))  # 式7-6
##   return y
sigmoid <- function(a) {
  y <- 1 / (1 + exp(-a))
  return(y)
}

## # ネットワーク  ----------
## def FNN(wv, M, K, x):
##   N, D = x.shape  # 入力次元
##   w = wv[: M * (D + 1)]     # 中間層ニューロンへの重み
##   w = w.reshape(M, (D + 1))
##   v = wv[M * (D + 1) :]     # 出力層ニューロンへの重み
##   v = v.reshape((K, M + 1))
##   b = np.zeros((N, M + 1))  # 中間層ニューロンの入力総和
##   z = np.zeros((N, M + 1))  # 中間層ニューロンの出力
##   a = np.zeros((N, K))      # 出力層ニューロンの入力総和
##   y = np.zeros((N, K))      # 出力層ニューロンの出力
##   for n in range(N):
##     # 式7-14、式7-15で中間層の出力zを計算
##     for m in range(M):
##       # (A) x[n, :]の末尾に1を加える
##       x_add1 = np.r_[x[n, :], 1]
##       b[n, m] = w[m, :] @ x_add1
##       z[n, m] = sigmoid(b[n, m])
##     # 式7-16、式7-17で出力層の出力yを計算
##     z[n, M] = 1  # ダミーニューロン
##   u = 0
##   for k in range(K):
##     a[n, k] = v[k, :] @ z[n, :]
##     u = u + np.exp(a[n, k])
##   for k in range(K):
##     y[n, k] = np.exp(a[n, k]) / u
## return y, a, z, b
FNN <- function(wv, M, K, x) {
  N <- nrow(x); D <- ncol(x)
  w <- wv[1:(M * (D + 1))]
  w <- array(w, dim = c(M, D + 1))
  v <- wv[(M * (D + 1) + 1):length(wv)]
  v <- array(v, dim = c(K, M + 1))
  b <- array(0, dim = c(N, M + 1))
  z <- array(0, dim = c(N, M + 1))
  a <- array(0, dim = c(N, K))
  y <- array(0, dim = c(N, K))
  for (n in 1:N) {
    for (m in 1:M) {
      x_add1 <- c(x[n,], 1)
      b[n, m] <- w[m,] %*% x_add1
      z[n, m] <- sigmoid(b[n, m])
    }
    z[n, M + 1] <- 1
    u <- 0
    for (k in 1:K) {
      a[n, k] <- v[k,] %*% z[n,]
      u <- u + exp(a[n, k])
    }
    for (k in 1:K) {
      y[n, k] <- exp(a[n, k]) / u
    }
  }
  return(list(y = y, a = a, z = z, b = b))
}

## # テスト ----------
## wv = np.ones(15)
wv <- rep(1, times = 15)
## M, K = 2, 3
M <- 2; K <- 3
## y, a, z, b = FNN(wv, M, K, X_train[:2, :])
res <- FNN(wv, M, K, X_train[1:2,])
## print("y =\n", np.round(y, 6))
print('y =')
print(round(res$y, 6))
## print("a =\n", np.round(a, 6))
print('a =')
print(round(res$a, 6))
## print("z =\n", np.round(z, 6))
print('z =')
print(round(res$z, 6))
## print("b =\n", np.round(b, 6))
print('b =')
print(round(res$b, 6))


# リスト 7-1-(5)
## # 平均交差エントロピー誤差 ----------
## def cee_FNN(wv, M, K, x, t):
##   N, D = x.shape
##   y, a, z, b = FNN(wv, M, K, x)
##   # (A) 式7-18の計算
##   cee = -(t.reshape(-1) @ np.log(y.reshape(-1))) / N
##   return cee
cee_FNN <- function(wv, M, K, x, t) {
  N <- nrow(x); D <- ncol(x)
  res <- FNN(wv, M, K, x)
  cee <- -()
}

# テスト ----------
wv = np.ones(15)
M, K = 2, 3
cee = cee_FNN(wv, M, K, X_train[:2, :], T_train[:2, :])
print(f"cee = {cee:.6}")