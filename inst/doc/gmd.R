## -----------------------------------------------------------------------------
library(Rcpp)
library(SA24204171)
library(here)

# 读取数据
xmatrix <- readRDS(system.file("data", "Designmatrix.rds", package = "SA24204171"))
ymatrix <- readRDS(system.file("data", "Responsevariable.rds", package = "SA24204171"))

# 检查是否有缺失值
if (any(is.na(xmatrix), na.rm = TRUE)) {
  stop("Designmatrix contains missing values!")
}
if (any(is.na(ymatrix), na.rm = TRUE)) {
  stop("Responsevariable contains missing values!")
}

# 转换为矩阵
X <- as.matrix(xmatrix)
Y <- as.matrix(ymatrix)

# 设置常数
c <- 0.05
n <- nrow(X)
m <- ncol(X)

# 确保 X 中没有缺失值
if (any(is.na(X), na.rm = TRUE)) {
  stop("X matrix contains missing values!")
}

# 中心化设计矩阵和响应变量，并确保结果中没有缺失值
X_centered <- design_matrix(X, c)
X_centered[is.na(X_centered)] <- 0  # 替换缺失值为0

if (any(is.na(X_centered), na.rm = TRUE)) {
  stop("X_centered contains missing values!")
}

Y_centered <- response_variable(Y, c)
Y_centered[is.na(Y_centered)] <- 0  # 替换缺失值为0

if (any(is.na(Y_centered), na.rm = TRUE)) {
  stop("Y_centered contains missing values!")
}

# 计算辅助矩阵并检查
H <- auxiliary_matrix_H(c, n)
H[is.na(H)] <- 0  # 替换缺失值为0

if (any(is.na(H), na.rm = TRUE)) {
  stop("Matrix H contains missing values!")
}

Q <- auxiliary_matrix_Q(m)
Q[is.na(Q)] <- 0  # 替换缺失值为0

if (any(is.na(Q), na.rm = TRUE)) {
  stop("Matrix Q contains missing values!")
}

# 检查 rank_XTHXQ
rank_XTHXQ <- rank_of_XTHXQ(X_centered, H, Q)
cat("Rank of XTHXQ: ", rank_XTHXQ, "\n")

