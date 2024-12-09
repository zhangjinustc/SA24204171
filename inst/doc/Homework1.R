## -----------------------------------------------------------------------------
# 设置参数
n <- 1000  # 投掷次数
p <- 0.5   # 正面出现的概率

# 生成模拟数据
set.seed(123)  # 设置随机种子，保证结果可重复
flips <- rbinom(n, size = 1, prob = p)

# 统计正面出现的次数
heads <- sum(flips == 1)

# 计算正面出现的频率
frequency_heads <- heads / n

# 输出结果
cat("在", n, "次投掷硬币中，正面出现的次数为:", heads, "\n")
cat("正面出现的频率为:", frequency_heads, "\n")

# 显示结果表格
print(table(flips))

# 显示数学公式的期望值计算
cat("根据二项分布，期望值为: E[X] = n * p =", n * p, "\n")

## ----fig.width=8, fig.height=6------------------------------------------------
# 设置图形的边距
par(mar = c(4, 4, 2, 1))  # 减小边距
# 设置图形排列为 2x2
par(mfrow = c(2, 2))

# 加载数据集
data(mtcars)
head(mtcars)

# 拟合线性回归模型
model <- lm(mpg ~ wt, data = mtcars)
summary(model)

# 绘制回归诊断图
plot(model)

## -----------------------------------------------------------------------------
# 数据准备

#我们将使用内置的 `iris` 数据集进行 K-means 聚类分析。

# 加载数据集
data(iris)
head(iris)

# 进行 K-means 聚类
set.seed(42)  # 设置随机种子以保证结果可重复
kmeans_result <- kmeans(iris[, -5], centers = 3)  # 去除类别列
table(kmeans_result$cluster, iris$Species)

library(ggplot2)
library(cluster)

# 主成分分析
pca <- prcomp(iris[, -5], scale. = TRUE)
pca_data <- data.frame(pca$x[, 1:2], cluster = as.factor(kmeans_result$cluster))

# 绘制聚类结果
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means 聚类结果", x = "主成分 1", y = "主成分 2") +
  theme_minimal()


