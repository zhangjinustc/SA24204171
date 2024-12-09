## -----------------------------------------------------------------------------
# 定义计算第k项的函数
compute_kth_term <- function(k, a, d) {
  # 检查输入有效性
  if (d < 1 || !is.numeric(a) || length(a) == 0) {
    stop("d 必须是一个大于或等于1的整数，a 必须是一个数值向量")
  }
  
  # 计算欧几里得范数
  norm_a <- sqrt(sum(a^2))
  
  # 计算各个部分的系数
  term1 <- ((-1)^k) / (factorial(k) * 2^k)
  term2 <- (norm_a^(2 * k + 2)) / ((2 * k + 1) * (2 * k + 2))
  gamma1 <- gamma((d + 1) / 2)
  gamma2 <- gamma(k + 3 / 2)
  gamma3 <- gamma(k + d / 2 + 1)
  
  # 计算第k项
  term <- term1 * term2 * (gamma1 * gamma2 / gamma3)
  return(term)
}

# 定义计算和的函数
compute_sum <- function(a, d, max_iter = 100) {
  sum_value <- 0
  for (k in 0:max_iter) {
    sum_value <- sum_value + compute_kth_term(k, a, d)
  }
  return(sum_value)
}

# 设置向量 a 和 d
a <- c(1, 2)  # 向量 a
d <- length(a)  # d 为向量 a 的维数

# 计算和
result <- compute_sum(a, d)
cat("当 a = (1, 2)^T 时的和为：", result, "\n")


## -----------------------------------------------------------------------------
# 加载必要的库
 install.packages("stats", dependencies=TRUE)

# 定义函数计算 S_{k-1}(a) 和 S_k(a)
S_k_minus_1 <- function(a, k) {
  qt_value <- sqrt((a^2 * (k - 1)) / (k - a^2))
  return(1 - pt(qt_value, df = k - 1))  # 使用 t 分布的累积分布函数
}

S_k <- function(a, k) {
  qt_value <- sqrt((a^2 * k) / (k + 1 - a^2))
  return(1 - pt(qt_value, df = k))
}

# 定义目标函数找到交点
find_intersection <- function(k) {
  # 目标函数是 S_k_minus_1(a, k) - S_k(a, k) = 0
  objective_function <- function(a) {
    abs(S_k_minus_1(a, k) - S_k(a, k))
  }
  
  # 在区间 (0, sqrt(k)) 上进行优化
  result <- optimize(objective_function, interval = c(0, sqrt(k)))
  return(result$minimum)  # 返回最小化目标函数的 a 值，即交点
}

# 计算 k = 4 到 25 以及 100, 500, 1000 的交点 A(k)
k_values <- c(4:25, 100, 500, 1000)
intersection_points <- sapply(k_values, find_intersection)

# 输出结果
result_df <- data.frame(k = k_values, A_k = intersection_points)
print(result_df)


## -----------------------------------------------------------------------------
# 加载必要的库
install.packages("stats", dependencies=TRUE)

# 定义 c_k 的表达式
ck_value <- function(a, k) {
  # 控制 ck 的最大值，避免数值过大
  ck <- sqrt((a^2 * k) / (k + 1 - a^2))
  
  # 限制 ck 的最大值，避免超出合理范围
  max_ck <- sqrt(k)  # 可根据实际情况调整此值
  if (ck > max_ck) {
    return(max_ck)
  }
  return(ck)
}

# 定义积分函数，左侧和右侧
integrate_function_left <- function(a, k) {
  ck_minus_1 <- ck_value(a, k - 1)
  integrand <- function(u) (1 + (u^2) / (k - 1))^(-k / 2)
  
  integral <- tryCatch({
    result <- integrate(integrand, lower = 0, upper = ck_minus_1)
    if (is.finite(result$value)) {
      return(result$value)
    } else {
      return(NA)
    }
  }, error = function(e) {
    return(NA)
  })
  
  if (is.na(integral)) return(NA)
  
  constant <- 2 * gamma(k / 2) / (sqrt(pi * (k - 1)) * gamma((k - 1) / 2))
  return(constant * integral)
}

integrate_function_right <- function(a, k) {
  ck <- ck_value(a, k)
  integrand <- function(u) (1 + (u^2) / k)^(-(k + 1) / 2)
  
  integral <- tryCatch({
    result <- integrate(integrand, lower = 0, upper = ck)
    if (is.finite(result$value)) {
      return(result$value)
    } else {
      return(NA)
    }
  }, error = function(e) {
    return(NA)
  })
  
  if (is.na(integral)) return(NA)
  
  constant <- 2 * gamma((k + 1) / 2) / (sqrt(pi * k) * gamma(k / 2))
  return(constant * integral)
}

# 目标函数，找到使左侧和右侧相等的a值
find_a_solution <- function(k) {
  objective_function <- function(a) {
    left_val <- integrate_function_left(a, k)
    right_val <- integrate_function_right(a, k)
    
    # 检查是否有无效值
    if (is.na(left_val) || is.na(right_val) || is.infinite(left_val) || is.infinite(right_val)) {
      return(Inf)
    }
    return(abs(left_val - right_val))
  }
  
  # 在合理范围内寻找a值，并处理异常情况
  result <- tryCatch({
    optimize(objective_function, interval = c(0.01, sqrt(k)), tol = 1e-6)
  }, warning = function(w) {
    message("警告：", conditionMessage(w))
    return(list(minimum = NA))
  }, error = function(e) {
    message("错误：", conditionMessage(e))
    return(list(minimum = NA))
  })
  
  return(result$minimum)
}

# 计算给定k值的a解
k_values <- c(4:25, 100, 500, 1000)
a_solutions <- sapply(k_values, find_a_solution)

# 计算 A(k)，假设 A(k) 已知
# 在实际问题中，A(k) 需要根据之前的交点计算得出
A_k_values <- sapply(k_values, function(k) sqrt(k) * 0.5)  # 这里只是一个示例，实际需要从之前的交点计算得出

# 比较解与 A(k) 的差异
comparison_df <- data.frame(k = k_values, a_solution = a_solutions, A_k = A_k_values, difference = abs(a_solutions - A_k_values))

# 打印比较结果
print(comparison_df)


## -----------------------------------------------------------------------------
# 定义E-M算法来估计lambda
EM_algorithm <- function(Y, tau, max_iter = 100, tol = 1e-6) {
  n <- length(Y)
  
  # 初始lambda估计
  lambda_est <- 1 / mean(Y[Y < tau])  # 这可以作为初始猜测
  
  for (iter in 1:max_iter) {
    # E步：计算期望
    expected_T <- rep(NA, n)
    for (i in 1:n) {
      if (Y[i] < tau) {
        # 如果是观测值，直接取值
        expected_T[i] <- Y[i]
      } else {
        # 如果是删失值，计算期望
        expected_T[i] <- tau + (1 / lambda_est)  # 删失数据的条件期望
      }
    }
    
    # M步：更新lambda
    lambda_new <- n / sum(expected_T)
    
    # 检查收敛
    if (abs(lambda_new - lambda_est) < tol) {
      cat("E-M算法收敛：", "lambda =", lambda_new, "\n")
      return(lambda_new)
    }
    
    lambda_est <- lambda_new
  }
  
  # 如果最大迭代次数结束仍未收敛
  cat("达到最大迭代次数：", "lambda =", lambda_est, "\n")
  return(lambda_est)
}

# 给定观察数据
Y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)

# 估计lambda
lambda_em <- EM_algorithm(Y, tau = 1)

cat("通过E-M算法估计的lambda为：", lambda_em, "\n")

# 比较MLE方法的估计值（MLE方法在删失数据下的估计值）
# 使用未删失数据的MLE
lambda_mle <- length(Y[Y < 1]) / sum(Y[Y < 1])
cat("通过MLE估计的lambda为：", lambda_mle, "\n")


