## chapter6.R
## Machine-Learning-with-T
## Nakano Keisuke

# 準備
library(tidyverse)


# リスト 6-1-(1)
## %matplotlib inline
## import numpy as np
## import matplotlib.pyplot as plt
NULL

## # データ生成 ----------
## np.random.seed(seed=0)             # 乱数を固定
set.seed(0)
## X_min, X_max = 0, 2.5              # x の上限と下限（表示用）
X_min <- 0; X_max <- 2.5
## N = 30                             # データ数
N <- 30
## col = ["cornflowerblue", "gray"]   # クラス0と1の表示色
col <- c('cornflowerblue', 'gray')
## X = np.zeros(N)                    # 空のXを準備
X <- rep(NA, times = N)
## T = np.zeros(N, dtype=np.uint8)    # 空のTを準備
t <- rep(NA, times = N)
## prm_s = [0.4, 0.8]                 # クラス0と1の分布の開始地点
prm_s <- c(0.4, 0.8)
## prm_w = [0.8, 1.6]                 # クラス0と1の分布の幅
prm_w = c(0.8, 1.6)
## prm_pi = 0.5                       # クラス0の全体に対する比率
prm_pi <- 0.5
## for n in range(N):
##   r = np.random.rand()
##   T[n] = 0 * (r < prm_pi) + 1 * (r >= prm_pi)          # (A)
##   X[n] = prm_s[T[n]] + np.random.rand() * prm_w[T[n]]  # (B)
for (n in 1:N) {
  r <- runif(1)
  t[n] <- 0 * (r < prm_pi) + 1 * (r >= prm_pi)
  X[n] <- prm_s[t[n] + 1] + runif(1) * prm_w[t[n] + 1]
}
# データ表示
## print("X =", np.round(X, 2))
cat('X =', round(X, 2))
## print("T =", T)
cat('T =', t)


# リスト 6-1-(2)
## # データ分布表示 ----------
## def show_data1d(x, t):
##   for k in range(2):  # (A) k=0、1 のループ
##     plt.plot(       # (B) 分布を表示
##       x[t == k], t[t == k], col[k],
##       alpha=0.5, linestyle="none", marker="o",
##     )
##   plt.xticks(np.arange(0, 3, 0.5))
##   plt.yticks([0, 1])
##   plt.xlim(X_min, X_max)
##   plt.ylim(-0.5, 1.5)
show_data1d <- function(x, t) {
  df <- tibble(x = x, t = t)
  p <- ggplot() +
    geom_point(aes(x = x, y = t,
                   color = as.factor(t))) +
    labs(x = element_blank(), y = element_blank()) +
    xlim(X_min, X_max) + ylim(0, 1) +
    coord_fixed(ratio = abs(X_min - X_max) / 1) +
    scale_y_continuous(breaks = c(0, 1),
                       labels = c(0, 1)) +
    theme(legend.position = 'none')
  return(p)
}

## # メイン ----------
## fig = plt.figure(ﬁgsize=(3, 3))
## show_data1d(X, T)
## plt.grid()
## plt.show()
show_data1d(X, t)


# リスト 6-1-(3)
## # ロジスティック回帰モデル ----------
## def logistic(x, w):
##   y = 1 / (1 + np.exp(-(w[0] * x + w[1])))  # 式6-10
##   return y
logistic <- function(x, w) {
  y <- 1 / (1 + exp(-(w[1] * x + w[2])))
  return(y)
}


# リスト 6-1-(4)
## # ロジスティック回帰モデルの表示 ----------
## def show_logistic(w):
##   x = np.linspace(X_min, X_max, 100)
##   y = logistic(x, w)
##   plt.plot(x, y, "gray", linewidth=4)
##   # 決定境界
##   i = np.min(np.where(y > 0.5))     # (A)
##   boundary = (x[i - 1] + x[i]) / 2  # (B)
##   plt.plot([boundary, boundary], [-0.5, 1.5], "black", linestyle="--")
##   return boundary
show_logistic <- function(w) {
  x <- seq(X_min, X_max, length.out = 100)
  y <- logistic(x, w)
  df <- tibble(x = x, y = y) |>
    mutate(id = 1:n())
  p <- df |>
    ggplot() +
    geom_line(aes(x = x, y = y),
              color = 'gray')
  i <- df |>
    filter(y > 0.5) |>
    filter(y == min(y))
  boundary <- (x[i$id - 1] + x[i$id]) / 2
  p <- p +
    geom_vline(xintercept = boundary, linetype = 'dashed')
  return(list(p = p, boundry = boundary))
}

## # テスト ----------
## w = np.array([8, -10])
w <- c(8, -10)
## b = show_logistic(w)
b <- show_logistic(w)
## ## print(f'decision boundary = {b}')
cat('dicision boundary =', b$boundry)
## ## plt.grid()
## ## plt.show()
plot(b$p)


# リスト 6-1-(5)
## # 平均交差エントロピー誤差 ----------
## def cee_logistic(w, x, t):
##   y = logistic(x, w)
##   # 式6-16の計算
##   cee = 0
##   for n in range(len(y)):
##     cee = cee - (t[n] * np.log(y[n]) + (1 - t[n]) * np.log(1 - y[n]))
##   cee = cee / N
##   return cee
cee_logistic <- function(w, x, t) {
  y <- logistic(x, w)
  cee <- 0
  for (n in 1:length(y)) {
    cee <- cee - (t[n] * log(y[n]) + (1 - t[n]) * log(1 - y[n]))
  }
  cee <- cee / N
  return(cee)
}

## # テスト ----------
## w = np.array([1, 1])
w <- c(1, 1)
## cee = cee_logistic(w, X, T)
cee <- cee_logistic(w, X, t)
## print(f"CEE = {cee:.6f}")
cat('CEE =', round(cee, 6))
