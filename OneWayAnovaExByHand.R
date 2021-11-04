a <- 3
row1 <- c(491.5, 498.3, 498.1, 493.5, 493.6)
row2 <- c(488.5, 484.65, 479.9, 477.35)
row3 <- c(490.1, 484.80, 488.25, 473, 471.85, 478.65)
N <- length(row1) + length(row2) + length(row3)

all <- c(row1, row2, row3)
mean.all <- mean(all)
mean.row1 <- mean(row1)
mean.row2 <- mean(row2)
mean.row3 <- mean(row3)

# SSA
SSA <- length(row1)*(mean.row1-mean.all)^2 + 
       length(row2)*(mean.row2-mean.all)^2 +
       length(row3)*(mean.row3-mean.all)^2

SSA
SSA/(a-1)

# SST
SST <- sum((all-mean.all)^2)
SST
SST/(N-1)

# SSE
SSE <- SST-SSA
SSE
SSE/(N-a)

# F_0
(SSA/(a-1))/(SSE/(N-a))

# 0.05 quantile of Fischer
qf(0.05, a-1, N-a, lower.tail = F)

# 0.05 of student Range (for Tukey test, comparison of means).
qtukey(0.05, 3, 12, lower.tail = F)
