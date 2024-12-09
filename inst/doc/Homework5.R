## -----------------------------------------------------------------------------
# 设置参数
N <- 1000
null_hypotheses <- 950
alternative_hypotheses <- 50
alpha <- 0.1
simulations <- 10000

# 存储结果
results <- matrix(0, nrow = 3, ncol = 2)
rownames(results) <- c("FWER", "FDR", "TPR")
colnames(results) <- c("Bonferroni correction", "B-H correction")

# 模拟
for (sim in 1:simulations) {
  # 生成p值
  p_values <- c(runif(null_hypotheses), rbeta(alternative_hypotheses, 0.1, 1))

  # Bonferroni调整
  bonferroni_p <- p.adjust(p_values, method = "bonferroni")
  
  # B-H调整
  bh_p <- p.adjust(p_values, method = "BH")

  # 计算FWER
  fwer_bonferroni <- mean(bonferroni_p < alpha)
  fwer_bh <- mean(bh_p < alpha)

  # 计算FDR和TPR
  # 真实的显著性结果
  true_significant <- c(rep(0, null_hypotheses), rep(1, alternative_hypotheses))
  
  # Bonferroni
  significant_bonferroni <- bonferroni_p < alpha
  fdr_bonferroni <- mean(significant_bonferroni) / mean(significant_bonferroni & true_significant)
  tpr_bonferroni <- mean(significant_bonferroni & true_significant) / alternative_hypotheses
  
  # B-H
  significant_bh <- bh_p < alpha
  fdr_bh <- mean(significant_bh) / mean(significant_bh & true_significant)
  tpr_bh <- mean(significant_bh & true_significant) / alternative_hypotheses

  # 汇总结果
  results[1, 1] <- results[1, 1] + fwer_bonferroni
  results[1, 2] <- results[1, 2] + fwer_bh
  results[2, 1] <- results[2, 1] + fdr_bonferroni
  results[2, 2] <- results[2, 2] + fdr_bh
  results[3, 1] <- results[3, 1] + tpr_bonferroni
  results[3, 2] <- results[3, 2] + tpr_bh
}

# 取平均
results <- results / simulations

# 输出结果
print(results)
# 虽然Bonferroni校正在控制FWER方面表现良好，但可能导致检验能力的损失。相比之下，B-H校正在控制FDR的同时，能够更有效地识别真实的信号。

## -----------------------------------------------------------------------------
# 加载必要的包
library(boot)

# 数据集
failure_times <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)

# 计算MLE
lambda_mle <- 1 / mean(failure_times)

# 定义bootstrap函数
bootstrap_function <- function(data, indices) {
  sample_data <- data[indices]
  return(1 / mean(sample_data))
}

# 执行bootstrap
set.seed(123)  # 设置随机种子以确保结果可重复
R <- 10000
bootstrap_results <- boot(data = failure_times, statistic = bootstrap_function, R = R)

# 计算偏差和标准误差
bootstrap_lambda <- bootstrap_results$t
bias <- mean(bootstrap_lambda) - lambda_mle
se <- sd(bootstrap_lambda)

# 输出结果
cat("最大似然估计 λ:", lambda_mle, "\n")
cat("偏差:", bias, "\n")
cat("标准误差:", se, "\n")


## -----------------------------------------------------------------------------
# 加载必要的包
library(boot)

# 数据集
failure_times <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)

# 计算MLE
lambda_mle <- 1 / mean(failure_times)

# 定义bootstrap函数
bootstrap_function <- function(data, indices) {
  sample_data <- data[indices]
  return(1 / mean(sample_data))
}

# 执行bootstrap
set.seed(123)  # 设置随机种子以确保结果可重复
R <- 10000
bootstrap_results <- boot(data = failure_times, statistic = bootstrap_function, R = R)

# 计算置信区间
normal_ci <- boot.ci(bootstrap_results, type = "norm")
basic_ci <- boot.ci(bootstrap_results, type = "basic")
percentile_ci <- boot.ci(bootstrap_results, type = "perc")
bca_ci <- boot.ci(bootstrap_results, type = "bca")

# 输出结果
cat("标准正态法置信区间:", normal_ci$norm[2], "至", normal_ci$norm[3], "\n")
cat("基本法置信区间:", basic_ci$basic[4], "至", basic_ci$basic[5], "\n")
cat("百分位法置信区间:", percentile_ci$perc[4], "至", percentile_ci$perc[5], "\n")
cat("BCa法置信区间:", bca_ci$bca[4], "至", bca_ci$bca[5], "\n")

# 不同方法得到的置信区间会有所不同，主要原因在于各方法对数据分布的假设和计算方式不同。
# 标准正态法假设bootstrap分布近似正态，并根据MLE计算置信区间。结果可能会受到样本大小和分布偏态的影响。
# 基本法直接基于bootstrap样本的经验分布，计算得到的置信区间更加依赖于数据的实际分布。
# 百分位法通过对bootstrap样本的百分位数进行计算，反映了数据的实际分布特征。这种方法不需要假设分布形态，通常适用于非正态数据。
# BCa法结合了偏差校正和加速方法，能提供更准确的置信区间，尤其在数据分布不对称时表现良好。

