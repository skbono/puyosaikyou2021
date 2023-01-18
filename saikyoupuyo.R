### ライブラリの読み込み ###
library(rstan)
library(bayesplot)
library(dplyr)
library(magrittr)
library(readr)



############# データ読み込み～データ加工 #####################----------------------------------------------------------

### データの読み込み（1行1連戦 形式） ###
dat <- read_csv("data/SaikyouPuyo2021.csv", locale = readr::locale(encoding = "CP932"))
dat %<>% select(P1, P2, score1, score2)


### 1行1連戦 → 1行1試合に加工 ###
W <- NULL
L <- NULL
for (i in 1:nrow(dat)) {
  
  # Winner列
  W_tmp1 <- rep(dat$P1[i], dat$score1[i])
  W_tmp2 <- rep(dat$P2[i], dat$score2[i])
  W <- c(W, W_tmp1, W_tmp2)
  
  # Loser列  
  L_tmp1 <- rep(dat$P2[i], dat$score1[i])
  L_tmp2 <- rep(dat$P1[i], dat$score2[i])
  L <- c(L, L_tmp1, L_tmp2)
}

puyoWL <- data.frame(L, W)







############# MCMCの実行まで #####################----------------------------------------------------------
### stan用にリスト化 ###
P <- c(puyoWL$L, puyoWL$W) %>% unique() %>% length()
G <- nrow(puyoWL)

data <- list(P=P, G=G, LW=puyoWL)

# mu[1]~[8]の順番は↓の順番
# c(puyoWL$L, puyoWL$W) %>% unique() %>% sort()


### MCMCの実行 ###
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

stanmodel <- stan_model(file='saikyoupuyo.stan')
fit <- sampling(stanmodel, 
                data=data, pars=c('mu','s_mu','s_pf'), 
                chain = 4,
                iter = 10000,
                warmup = 5000,
                thin = 3,
                seed = 1)





############# サンプリングの結果 #####################----------------------------------------------------------
### 事後分布の範囲 ###
# 「強さ」
mcmc_intervals(fit,
               pars = c("mu[2]", "mu[6]", "mu[4]", "mu[5]", "mu[1]", "mu[3]", "mu[8]", "mu[7]"),
               prob = 0.5,
               prob_outer = 0.95)

# 「勝負ムラ」
mcmc_intervals(fit,
               pars = c("s_pf[2]", "s_pf[6]", "s_pf[4]", "s_pf[5]", "s_pf[1]", "s_pf[3]", "s_pf[8]", "s_pf[7]"),
               prob = 0.5,
               prob_outer = 0.95)




########## 記事用のおまけ ###########---------------------
### 勝率 ###
w_rate <- data.frame(P     = c(dat$P1, dat$P2),
                     Score = c(dat$score1, dat$score2))
w_rate %>% 
  mutate(win = if_else(Score == 30, 1, 0)) %>% 
  group_by(P) %>% 
  summarise(w_rate = mean(win)) %>% 
  ggplot() +
  geom_bar (aes(x = reorder(P, -w_rate), y = w_rate), stat = "identity") +
  geom_text(aes(x = reorder(P, -w_rate), y = w_rate, label = w_rate, vjust = -0.5), size = 5) +
  ylab("") + xlab("") +
  ylim(c(0,1)) +
  theme(text = element_text(size = 18))


