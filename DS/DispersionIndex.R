##  ソフトウェアフォールト個数データ（DS1〜DS4）を
##  特徴量から 2 パターンに分類する R スクリプト

##  前提：
##    - 作業ディレクトリに次の4ファイルがある
##        DS1.txt, DS2.txt, DS3.txt, DS4.txt
##    - 形式：1列目 = 時刻インデックス, 2列目 = 区間ごとの故障個数

##  出力：
##    - 各 DS の特徴量とクラスタ結果をまとめた data.frame をコンソール表示
##    - "DS_features_clusters.csv" にも保存

# txt ファイルから time, count だけを取り出す共通関数
load_fault_txt <- function(file) {
        dat_raw <- read.table(file, header = FALSE)
        
        if (ncol(dat_raw) < 2) {
                stop("少なくとも2列（time と count）が必要です: ", file)
        }
        
        # 1列目 = time, 2列目 = 区間ごとの故障個数 とみなす
        dat <- dat_raw[, 1:2]
        colnames(dat) <- c("time", "count")
        
        return(dat)
}

# 前半にどれだけ故障が出たかを表す Early Fault Index
## early_prop = 0.5 なら「観測時間の前半」の割合
get_early_index <- function(file, early_prop = 0.5) {
        dat <- load_fault_txt(file)  # ←ここだけ修正
        
        T_max  <- max(dat$time)
        cutoff <- T_max * early_prop
        
        early_flag <- dat$time <= cutoff
        I_early <- sum(dat$count[early_flag]) / sum(dat$count)
        
        return(I_early)
}

## ブロックごとの個数の分散 / 平均（dispersion index）
## block_size 区間ごとにまとめて、その合計個数の変動を見る
get_dispersion_index <- function(file, block_size = 5) {
        dat <- load_fault_txt(file)  # ←ここだけ修正
        
        block <- ceiling(dat$time / block_size)
        agg_counts <- tapply(dat$count, block, sum)
        
        m <- mean(agg_counts)
        v <- var(agg_counts)
        
        D <- v / m
        return(D)
}

## 必要ならここを自分のファイル名に書き換える
files <- c("DS1.txt", "DS2.txt", "DS3.txt", "DS4.txt")

ds_names <- gsub("\\.txt$", "", files)

## Early Fault Index（観測前半での故障割合）
I_early_vec <- sapply(files, get_early_index, early_prop = 0.5)

## Dispersion index（バースト度合い）
D_vec <- sapply(files, get_dispersion_index, block_size = 5)

## 特徴量をまとめたデータフレーム
features <- data.frame(
        Dataset = ds_names,
        I_early = I_early_vec,
        Dispersion = D_vec,
        stringsAsFactors = FALSE
)

## (A) I_early の中央値で 2 パターンに分ける（早期集中型 vs 後期型）
med_I <- median(features$I_early)
features$EarlyType <- ifelse(features$I_early > med_I,
                             "Early-concentrated",  # 早期集中型
                             "Late-type")          # 後期型

## (B) I_early と Dispersion の 2次元特徴量で k-means (2クラスタ)
set.seed(123)  # 再現性のため固定
km <- kmeans(features[, c("I_early", "Dispersion")], centers = 2)

features$ClusterID <- km$cluster

## クラスタをラベルに変換
## I_early と Dispersion の平均が大きい方を "Type1" とする例
cluster_summary <- aggregate(features[, c("I_early", "Dispersion")],
                             by = list(ClusterID = features$ClusterID),
                             FUN = mean)

## 平均 I_early が大きいクラスタを "Type1" にする
ordered_clusters <- cluster_summary$ClusterID[order(cluster_summary$I_early,
                                                    decreasing = TRUE)]

label_map <- setNames(
        object = c("Type1_highEarly", "Type2_lowEarly"),
        nm     = ordered_clusters
)

features$ClusterLabel <- label_map[as.character(features$ClusterID)]

cat("=== Data set features and 2-pattern classification ===\n\n")
print(features)

## CSV として保存（論文や解析用に）
write.csv(features, file = "DS_features_clusters.csv", row.names = FALSE)

cat("\n結果を 'DS_features_clusters.csv' に書き出しました。\n")
cat("この表をもとに、2パターン（EarlyType や ClusterLabel）を論文中で定義できます。\n")
