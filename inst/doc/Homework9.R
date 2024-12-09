## -----------------------------------------------------------------------------
library(lpSolve)

# 定义目标函数系数
objective <- c(4, 2, 9)

# 定义约束矩阵（每一行对应一个约束条件）
# 约束条件为 2x + y + z <= 2 和 x - y + 3z <= 3
constraints <- matrix(c(2, 1, 1,
                        1, -1, 3), nrow = 2, byrow = TRUE)

# 定义约束条件的右侧值
rhs <- c(2, 3)

# 定义约束条件类型
# "<=" 表示小于等于约束
direction <- c("<=", "<=")

# 使用lp函数求解
solution <- lp(direction = "min", objective.in = objective, const.mat = constraints, const.dir = direction, const.rhs = rhs)

# 输出结果
solution$solution  # 最优解
solution$objval    # 最小目标值


## -----------------------------------------------------------------------------
# 定义公式列表
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# 使用for循环拟合线性模型
models_for <- list()  # 存储拟合结果
for (i in 1:length(formulas)) {
  models_for[[i]] <- lm(formulas[[i]], data = mtcars)
}

# 查看每个模型的摘要
for (i in 1:length(models_for)) {
  cat("Model", i, "summary:\n")
  print(summary(models_for[[i]]))
  cat("\n\n")
}


## -----------------------------------------------------------------------------
# 使用lapply()拟合线性模型
models_lapply <- lapply(formulas, function(formula) lm(formula, data = mtcars))

# 查看每个模型的摘要
lapply(models_lapply, summary)

# 对比结果：for循环通常更直观，但在R语言中，lapply提供了更简洁和函数式编程的方式。

## -----------------------------------------------------------------------------
# 生成bootstraps列表
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

# 使用for循环对每个bootstrap样本拟合线性模型
models_for <- list()
for (i in 1:length(bootstraps)) {
  models_for[[i]] <- lm(mpg ~ disp, data = bootstraps[[i]])
}

# 查看每个模型的摘要
for (i in 1:length(models_for)) {
  cat("Bootstrap replicate", i, "summary:\n")
  print(summary(models_for[[i]]))
  cat("\n\n")
}
# 使用for循环我们直接在循环中拟合每个模型，并将结果存储在一个列表中。

## -----------------------------------------------------------------------------
# 定义拟合模型的函数
fit_model <- function(data) {
  lm(mpg ~ disp, data = data)
}

# 使用lapply()对每个bootstrap样本拟合线性模型
models_lapply <- lapply(bootstraps, fit_model)

# 查看每个模型的摘要
lapply(models_lapply, summary)
# 使用lapply通过事先定义一个函数，我们避免了使用匿名函数，保持代码的清晰性和可重用性。

## -----------------------------------------------------------------------------
# 定义计算R2的函数
rsq <- function(mod) summary(mod)$r.squared

# 使用for循环拟合线性模型并提取R2值
r_squared_for <- numeric(length(models_for))
for (i in 1:length(models_for)) {
  r_squared_for[i] <- rsq(models_for[[i]])
}

# 输出每个模型的R2值
cat("R2 values for each model (using for loop):\n")
print(r_squared_for)


## -----------------------------------------------------------------------------
# 使用lapply()提取每个模型的R2值
r_squared_lapply <- lapply(models_lapply, rsq)

# 输出每个模型的R2值
cat("R2 values for each model (using lapply):\n")
print(unlist(r_squared_lapply))


## -----------------------------------------------------------------------------
# 模拟100次t检验
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)

# 第一部分：使用sapply()和匿名函数提取每次试验的p值
p_values <- sapply(trials, function(x) x$p.value)

# 输出提取的p值
cat("使用sapply()和匿名函数提取的p值：\n")
print(p_values)

# 第二部分：去除匿名函数，使用[[直接提取p值
p_values_no_anon <- sapply(trials, function(x) x[["p.value"]])

# 输出提取的p值
cat("\n去除匿名函数，使用[[直接提取的p值：\n")
print(p_values_no_anon)


## -----------------------------------------------------------------------------
# 实现并行lapply的变体
parallel_lapply <- function(FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
  # 使用Map并行应用函数
  result_list <- Map(FUN, ...)
  
  # 使用vapply确保输出为向量或矩阵（根据simplify参数）
  if (simplify) {
    result_vector <- vapply(result_list, identity, FUN.VALUE = result_list[[1]])
    return(result_vector)
  } else {
    return(result_list)
  }
}

# 示例函数
my_function <- function(x, y) {
  return(x + y)
}

# 测试parallel_lapply函数
x <- 1:5
y <- 6:10

# 使用parallel_lapply进行并行处理
result <- parallel_lapply(my_function, x, y)

# 输出结果
print(result)
# 我们通过 Map() 并行地处理多个输入，并使用 vapply() 来确保输出的类型一致。
# 函数接受函数 FUN 和多个输入（通过 ... 传递），并允许用户控制输出是否简化为向量或矩阵。

## -----------------------------------------------------------------------------
# 计算卡方统计量的函数
fast_chisq_test <- function(x, y) {
  # 检查输入是否为数字或字符型数据
  if (!(is.numeric(x) || is.factor(x) || is.character(x))) {
    stop("First input must be a numeric, factor, or character vector.")
  }
  if (!(is.numeric(y) || is.factor(y) || is.character(y))) {
    stop("Second input must be a numeric, factor, or character vector.")
  }
  
  # 如果是字符型数据，将其转换为因子型
  if (is.character(x)) {
    x <- factor(x)
  }
  if (is.character(y)) {
    y <- factor(y)
  }
  
  # 创建一个列联表
  obs_table <- table(x, y)
  
  # 计算期望频数
  n <- sum(obs_table)  # 总样本数
  row_totals <- rowSums(obs_table)  # 行总和
  col_totals <- colSums(obs_table)  # 列总和
  expected <- outer(row_totals, col_totals, FUN = "*") / n  # 期望频数
  
  # 计算卡方统计量
  chi_squared_stat <- sum((obs_table - expected)^2 / expected)
  
  return(chi_squared_stat)
}

# 测试数据
x <- c(1, 1, 2, 2, 3, 3, 3, 4, 4)
y <- c("A", "B", "A", "B", "A", "B", "A", "B", "B")

# 计算卡方统计量
result <- fast_chisq_test(x, y)
cat("Calculated Chi-square statistic: ", result, "\n")


## -----------------------------------------------------------------------------
# 自定义的更快版本的 table 函数
fast_table <- function(x, y) {
  # 确保 x 和 y 是整数向量
  if (!is.integer(x)) x <- as.integer(x)
  if (!is.integer(y)) y <- as.integer(y)
  
  # 获取 x 和 y 的唯一值
  unique_x <- unique(x)
  unique_y <- unique(y)
  
  # 创建一个空矩阵来存储频数
  count_matrix <- matrix(0, nrow = length(unique_x), ncol = length(unique_y))
  
  # 使用索引填充矩阵
  for (i in seq_along(x)) {
    row_index <- which(unique_x == x[i])
    col_index <- which(unique_y == y[i])
    count_matrix[row_index, col_index] <- count_matrix[row_index, col_index] + 1
  }
  
  # 将结果的矩阵转换为数据框，类似于 table 输出
  dimnames(count_matrix) <- list(unique_x, unique_y)
  
  return(count_matrix)
}

# 优化后的卡方检验
fast_chisq_test <- function(x, y) {
  # 检查输入是否为整数向量
  if (!(is.integer(x) || is.numeric(x))) stop("First input must be an integer or numeric vector.")
  if (!(is.integer(y) || is.numeric(y))) stop("Second input must be an integer or numeric vector.")
  
  # 创建列联表
  obs_table <- fast_table(x, y)
  
  # 计算期望频数
  n <- sum(obs_table)  # 总样本数
  row_totals <- rowSums(obs_table)  # 行总和
  col_totals <- colSums(obs_table)  # 列总和
  expected <- outer(row_totals, col_totals, FUN = "*") / n  # 期望频数
  
  # 计算卡方统计量
  chi_squared_stat <- sum((obs_table - expected)^2 / expected)
  
  return(chi_squared_stat)
}

# 测试数据
x <- as.integer(c(1, 1, 2, 2, 3, 3, 3, 4, 4))
y <- as.integer(c(1, 2, 1, 2, 1, 2, 1, 2, 2))

# 计算卡方统计量
result <- fast_chisq_test(x, y)
cat("Calculated Chi-square statistic: ", result, "\n")


