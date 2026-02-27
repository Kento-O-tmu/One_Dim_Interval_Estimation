## 1列目=time, 2列目=count にそろえて読む（DS4の3列目は無視）
load_fault_txt <- function(file) {
        dat_raw <- read.table(file, header = FALSE)
        dat <- dat_raw[, 1:2]
        colnames(dat) <- c("time", "count")
        dat
}

files    <- c("DS1.txt", "DS2.txt", "DS3.txt", "DS4.txt")
ds_names <- gsub("\\.txt$", "", files)

## 2×2 でプロット
par(mfrow = c(2, 2),
    mar = c(4, 4, 2, 1))  # 余白（下, 左, 上, 右）

for (i in seq_along(files)) {
        dat <- load_fault_txt(files[i])
        time  <- dat$time
        count <- dat$count
        
        ## 棒グラフを描いて、そのx座標（棒の中心）を取得
        mids <- barplot(count,
                        names.arg = time,
                        space     = 0,
                        xlab      = "Time (index)",
                        ylab      = "Faults per interval",
                        main      = ds_names[i])
        
        ## 折れ線＋点を上から重ねる
        lines(mids, count)
        points(mids, count, pch = 16)
}
png("DS1to4_counts_bar_line_2x2.png",
    width = 1600, height = 1200, res = 200)

par(mfrow = c(2, 2),
    mar = c(4, 4, 2, 1))

for (i in seq_along(files)) {
        dat <- load_fault_txt(files[i])
        time  <- dat$time
        count <- dat$count
        
        mids <- barplot(count,
                        names.arg = time,
                        space     = 0,
                        xlab      = "Time (index)",
                        ylab      = "Faults per interval",
                        main      = ds_names[i])
        
        lines(mids, count)
        points(mids, count, pch = 16)
}

dev.off()
