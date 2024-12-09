## -----------------------------------------------------------------------------
# 假设已知的协方差矩阵 Sigma
Sigma <- matrix(c(4, 2, 1, 0, 0,
                  2, 3, 1, 0, 0,
                  1, 1, 2, 1, 0,
                  0, 0, 1, 2, 1,
                  0, 0, 0, 1, 1), 
                nrow = 5, byrow = TRUE)

# 计算特征值
eigenvalues <- eigen(Sigma)$values

# 计算样本估计 theta_hat
theta_hat <- eigenvalues[1] / sum(eigenvalues)

# Jackknife 方法计算偏差和标准误差
n <- length(eigenvalues)
jackknife_estimates <- numeric(n)

for (i in 1:n) {
  # 排除第 i 个特征值
  jackknife_values <- eigenvalues[-i]
  jackknife_estimates[i] <- jackknife_values[1] / sum(jackknife_values)
}

# 计算偏差和标准误差
jackknife_bias <- (n - 1) * (mean(jackknife_estimates) - theta_hat)
jackknife_se <- sqrt((n - 1) * var(jackknife_estimates))

# 输出结果
cat("样本估计 θ_hat:", theta_hat, "\n")
cat("Jackknife偏差:", jackknife_bias, "\n")
cat("Jackknife标准误差:", jackknife_se, "\n")


## -----------------------------------------------------------------------------
# 设置UTF-8编码
options(encoding = "UTF-8")

# 示例数据
set.seed(123)
n <- 100
magnetic <- rnorm(n, mean = 5, sd = 1)
chemical <- rnorm(n, mean = 50, sd = 10)

# 检查数据是否存在
if (!exists("magnetic") || !exists("chemical")) {
  stop("请确保'magnetic'和'chemical'向量已经定义。")
}

# 数据长度
n <- length(magnetic) 

# 初始化误差向量
e1 <- e2 <- e3 <- e4 <- e5 <- numeric(n) 

# 交叉验证
for (k in 1:n) {
  y <- magnetic[-k]
  x <- chemical[-k]
  
  # 线性模型
  J1 <- lm(y ~ x)
  yhat1 <- predict(J1, newdata = data.frame(x = chemical[k]))
  e1[k] <- magnetic[k] - yhat1
  
  # 二次模型
  J2 <- lm(y ~ x + I(x^2))
  yhat2 <- predict(J2, newdata = data.frame(x = chemical[k]))
  e2[k] <- magnetic[k] - yhat2
  
  # 立方模型
  J5 <- lm(y ~ x + I(x^2) + I(x^3))
  yhat5 <- predict(J5, newdata = data.frame(x = chemical[k]))
  e5[k] <- magnetic[k] - yhat5
  
  # 对数模型
  J3 <- lm(log(y) ~ x)
  yhat3 <- exp(predict(J3, newdata = data.frame(x = chemical[k])))
  e3[k] <- magnetic[k] - yhat3
  
  # 对数-对数模型
  J4 <- lm(log(y) ~ log(x))
  yhat4 <- exp(predict(J4, newdata = data.frame(x = log(chemical[k]))))
  e4[k] <- magnetic[k] - yhat4
}

# 计算均方误差
mse <- c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2), mean(e5^2))
cat("均方误差：\n")
cat(mse, "\n")

# 计算每个模型的调整R²
adj_r_squared <- c(summary(J1)$adj.r.squared,
                   summary(J2)$adj.r.squared,
                   summary(J3)$adj.r.squared,
                   summary(J4)$adj.r.squared,
                   summary(J5)$adj.r.squared)

cat("调整R²：\n")
cat(adj_r_squared, "\n")

# 选择最佳模型
best_model_cv <- which.min(mse)
best_model_adj_r2 <- which.max(adj_r_squared)

cat("通过交叉验证选择的最佳模型索引：", best_model_cv, "\n")
cat("通过最大调整R²选择的最佳模型索引：", best_model_adj_r2, "\n")



## -----------------------------------------------------------------------------
# 加载必要的库
library(DAAG)

# 加载数据集
data(ironslag)

# 查看数据集的结构
cat("数据集的结构:\n")
str(ironslag)  # 查看数据结构

# 提取磁性权重
x <- ironslag$magnetic[1:27]  # 假设前一半作为一个组
y <- ironslag$magnetic[28:53]  # 假设后一半作为另一个组

# 检查提取的数据
cat("\n组1的权重:", x, "\n")
cat("组2的权重:", y, "\n")

# 去除缺失值
x <- na.omit(x)
y <- na.omit(y)

# 打印非缺失值的数量
cat("组1的非缺失值数量:", length(x), "\n")
cat("组2的非缺失值数量:", length(y), "\n")

# 如果任一组的非缺失值数量为0，则退出
if (length(x) == 0 || length(y) == 0) {
  stop("其中一个组没有非缺失值，无法进行检验。")
}

# Cramér-von Mises 检验的函数
cramer_von_mises_test <- function(x, y, R = 999) {
  n <- length(x)
  m <- length(y)
  z <- c(x, y)
  
  # 计算观察到的 Cramér-von Mises 统计量
  ecdf_x <- ecdf(x)
  ecdf_y <- ecdf(y)
  D0 <- sum((ecdf_x(z) - ecdf_y(z))^2) * (n * m) / (n + m)^2
  
  # 存储置换统计量
  D <- numeric(R)
  
  # 进行置换检验
  for (i in 1:R) {
    k <- sample(1:(n + m), size = n, replace = FALSE)  # 随机选择索引
    x1 <- z[k]
    y1 <- z[-k]
    
    ecdf_x1 <- ecdf(x1)
    ecdf_y1 <- ecdf(y1)
    D[i] <- sum((ecdf_x1(z) - ecdf_y1(z))^2) * (length(x1) * length(y1)) / (length(x1) + length(y1))^2
  }
  
  # 计算 p 值
  p_value <- mean(c(D0, D) >= D0)
  
  return(list(statistic = D0, p_value = p_value))
}

# 应用 Cramér-von Mises 检验
result <- cramer_von_mises_test(x, y)

# 输出结果
cat("观察到的 Cramér-von Mises 统计量:", result$statistic, "\n")
cat("p 值:", result$p_value, "\n")


## -----------------------------------------------------------------------------
# 设置随机种子以便重现结果
set.seed(123)

# 生成示例数据
n <- 30  # 样本大小
x <- rnorm(n)  # 正态分布随机变量
y <- rnorm(n)  # 另一个正态分布随机变量

# 计算观察到的斯皮尔曼等级相关性
obs_cor <- cor(x, y, method = "spearman")
cat("观察到的斯皮尔曼等级相关性:", obs_cor, "\n")

# 进行置换检验
permutation_test <- function(x, y, R = 999) {
  n <- length(x)
  obs_cor <- cor(x, y, method = "spearman")
  count <- 0
  
  for (i in 1:R) {
    # 随机打乱 y 的顺序
    y_permuted <- sample(y)
    perm_cor <- cor(x, y_permuted, method = "spearman")
    
    # 计算大于等于观察到的相关性的次数
    if (abs(perm_cor) >= abs(obs_cor)) {
      count <- count + 1
    }
  }
  
  # 计算 p 值
  p_value <- count / R
  return(p_value)
}

# 运行置换检验
perm_p_value <- permutation_test(x, y)
cat("置换检验的 p 值:", perm_p_value, "\n")

# 使用 cor.test 进行常规斯皮尔曼检验
cor_test_result <- cor.test(x, y, method = "spearman")
cat("cor.test 的 p 值:", cor_test_result$p.value, "\n")


