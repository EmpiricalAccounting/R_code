# 第6章 記述統計を用いた利益分布アプローチ


# パッケージの読み込み
library(tidyverse)

# データの読み込み
financial_data <- read_csv("ch06_benchmark_analysis.csv")
head(financial_data)

# ROAとWACの計算
financial_data <- financial_data |>
  mutate(roa = earnings / lag_total_assets,
         wac = delta_working_capital / lag_total_assets)

# ROAのヒストグラムの作成
financial_data |>
  ggplot() +
  geom_histogram(aes(x = roa),
                 # ビンの範囲（binwidth）と0を通るように指定（boundary）
                 binwidth = 0.008, boundary = 0,
                 # ビンは灰色（fill）で黒枠（color）
                 fill = "lightgray", color = "black") +
  # 表示させる範囲の指定
  scale_x_continuous(limits = c(-0.08, 0.08)) +
  # ラベルの設定
  labs(title = "Histogram of ROA", x = "ROA", y = "Count") +
  # シンプルな表示とする
  theme_classic()

# ROAのヒストグラムで[0,0.008]のビンの色を黒色にする
financial_data |>
  # 黒色にしたい範囲[0,0.008]のROAの観測値にTRUEを振る
  mutate(highlight = between(roa, 0, 0.008)) |>
  ggplot() +
  geom_histogram(aes(x = roa, fill = highlight),
                 binwidth = 0.008, boundary = 0, color = "black") +
  # highlightに該当する範囲を黒色とする
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "lightgray"),
                    guide = "none") +
  scale_x_continuous(limits = c(-0.08, 0.08)) +
  labs(title = "Histogram of ROA", x = "ROA", y = "Count") +
  theme_classic()

# ビンの区切りを作成する
bin_breaks <- seq(-0.08, 0.08, by = 0.008)

# ROAのビンの番号を振る
financial_data <- financial_data |>
  mutate(bin = cut(roa, breaks = bin_breaks,
                   include.lowest = TRUE, labels = FALSE))

# WACの四分位点をビンごとに計算
bin_quantiles <- financial_data |>
  summarize(q_25 = quantile(wac, 0.25, na.rm = TRUE),
            q_50 = quantile(wac, 0.5,  na.rm = TRUE),
            q_75 = quantile(wac, 0.75, na.rm = TRUE),
            # グループの指定
            .by = bin) |>
  # ビンの中央値
  mutate(bin_mid = (bin_breaks[bin]
                    + bin_breaks[bin + 1]) / 2)

# 四分位点ごとにWACの折れ線グラフを作成
bin_quantiles |>
  ggplot(aes(x = bin_mid)) +
  # 四分位ごとの折れ線グラフ
  geom_line(aes(y = q_25, linewidth = "25%"), color = "black") +
  geom_line(aes(y = q_50, linewidth = "50%"), color = "black") +
  geom_line(aes(y = q_75, linewidth = "75%"), color = "black") +
  # ROAの0.004に縦線を引く
  geom_vline(xintercept = 0.004, color = "black",
             linetype = "dashed", linewidth = 1) +
  # 四分位ごとに線幅の指定
  scale_linewidth_manual(name = "Quantile",
                         values = c("25%" = 0.5,
                                    "50%" = 1.2,
                                    "75%" = 2)) +
  labs(title = "Working Capital by ROA Bins",
       x     = "ROA Bin (median)",
       y     = "Working Capital") +
  theme_classic()
