library(foreach)
library(doParallel)
library(surveillance)
library(EpiEstim)
library(forecast)
library(parallel)

# =========================================================
# Parallel setup
# =========================================================
num_cores <- parallel::detectCores()
cl <- parallel::makeCluster(num_cores)
doParallel::registerDoParallel(cl)

# =========================================================
# Holiday-adjusted 7-day moving average
# =========================================================
sevenmaho <- function(dat, label) {
  n <- length(dat)
  
  if (length(label) != n) {
    stop("dat and label must have the same length.")
  }
  
  if (n == 0) {
    return(numeric(0))
  }
  
  early_ma <- sapply(seq_len(min(6, n)), function(i) mean(dat[1:i]))
  
  if (n < 7) {
    return(early_ma)
  }
  
  holiday_ma <- numeric(n - 6)
  
  for (i in 7:n) {
    idx <- (i - 6):i
    dat_window <- dat[idx]
    lab_window <- label[idx]
    
    non_work_sum <- sum(dat_window[lab_window == 0])
    work_sum     <- sum(dat_window[lab_window != 0])
    
    non_work_n <- sum(lab_window == 0)
    work_n     <- sum(lab_window != 0)
    
    adj_sum <- 0
    
    if (work_n > 0) {
      adj_sum <- adj_sum + work_sum * 5 / work_n
    }
    if (non_work_n > 0) {
      adj_sum <- adj_sum + non_work_sum * 2 / non_work_n
    }
    
    holiday_ma[i - 6] <- adj_sum / 7
  }
  
  c(early_ma, holiday_ma)
}

# =========================================================
# New DGP
# =========================================================
simulate_series_dgp2 <- function(scale = 5) {
  label1 <- c(4, 5, 6, 7, 1, 2, 3)
  label <- rep(label1, 70)
  for (i in 1:20) {
    label[30 * i] <- 7
  }
  
  label3 <- c(1, 1, 0, 0, 1, 1, 1)
  label3 <- rep(label3, 70)
  for (i in 1:20) {
    label3[30 * i] <- 0
  }
  
  y <- c(5, 5, 5)
  
  for (i in 4:330) {
    if (i < 201) {
      y[i] <- y[i - 1] * 0.55 + y[i - 2] * 0.30 + rnorm(1, mean = 0, sd = 1) + 2
    } else if (i < 281) {
      y[i] <- y[i - 1] * 0.55 + y[i - 2] * 0.30 + rnorm(1, mean = 0, sd = 1) + 2 * 0.96^((i - 200) / 3)
    } else {
      y[i] <- y[i - 1] * 0.55 + y[i - 2] * 0.30 + rnorm(1, mean = 0, sd = 1) + 0.6733841 * 1.05^((i - 280) / 3)
    }
  }
  
  for (i in 4:330) {
    y[i] <- y[i] + 1 / label[i]
  }
  
  for (i in 150:280) {
    if (i %% 10 == 0) {
      y[i] <- y[i] + 3
    }
  }
  
  y[y < 0] <- 0
  
  y <- y[71:330] * scale
  label <- label[71:330]
  label3 <- label3[71:330]
  y_smooth <- sevenmaho(y, label3)
  y_smooth[y_smooth < 0] <- 0
  
  list(
    raw = y,
    smooth = y_smooth,
    label = label,
    holiday_label = label3
  )
}

# =========================================================
# Utility functions
# =========================================================
get_timeliness <- function(signals, t1, t2, detect_start = 211, detect_end = 260) {
  idx <- which(signals[detect_start:detect_end] == 1)[1]
  
  if (is.na(idx)) {
    first_alarm <- t2
  } else {
    first_alarm <- idx + detect_start - 1
  }
  
  (t2 - first_alarm) / (t2 - t1)
}

get_alarm_rate <- function(signals, bg_index = 12:210) {
  mean(signals[bg_index] == 1, na.rm = TRUE)
}

clean_result <- function(x) {
  x[x < 0] <- 0
  x[is.na(x)] <- 0
  x
}

# =========================================================
# EARS helpers
# =========================================================
run_ears_signal <- function(series, method, alpha, baseline = 7) {
  offset <- switch(method, C1 = 1, C2 = 3, C3 = 5)
  pad    <- switch(method, C1 = baseline, C2 = baseline + 2, C3 = baseline + 4)
  
  sts_obj <- sts(observed = series, frequency = 365)
  control <- list(
    range    = (baseline + offset):length(series),
    method   = method,
    baseline = baseline,
    alpha    = alpha
  )
  
  surv_obj <- earsC(sts_obj, control = control)
  out <- c(rep(0, pad), alarms(surv_obj)[, 1] * 1)
  out[is.na(out)] <- 0
  out
}

run_ears_panel <- function(series, alpha_vec, baseline = 7) {
  methods <- c("C1", "C2", "C3")
  lapply(seq_along(methods), function(i) {
    run_ears_signal(
      series = series,
      method = methods[i],
      alpha = alpha_vec[i],
      baseline = baseline
    )
  })
}

run_one_ears_sim_dgp2 <- function(alpha_raw, alpha_smooth, scale = 5) {
  sim <- simulate_series_dgp2(scale = scale)
  y  <- sim$raw
  y1 <- sim$smooth
  
  t1 <- 211
  t2_raw <- max(212, which.max(y[211:260]) + 210)
  t2_s   <- max(212, which.max(y1[211:260]) + 210)
  
  sig_raw <- run_ears_panel(y, alpha_vec = alpha_raw)
  sig_s   <- run_ears_panel(y1, alpha_vec = alpha_smooth)
  
  signals <- c(sig_raw, sig_s)
  
  alarm_rate <- sapply(signals, get_alarm_rate)
  
  timeliness <- c(
    sapply(sig_raw, get_timeliness, t1 = t1, t2 = t2_raw),
    sapply(sig_s,   get_timeliness, t1 = t1, t2 = t2_s)
  )
  
  c(alarm_rate, timeliness)
}

run_ears_simulation_dgp2 <- function(totaliter, alpha_raw, alpha_smooth, scale = 5) {
  foreach(
    jj = 1:totaliter,
    .combine = "rbind",
    .packages = c("stats", "surveillance"),
    .export = c(
      "simulate_series_dgp2",
      "sevenmaho",
      "get_timeliness",
      "get_alarm_rate",
      "run_ears_signal",
      "run_ears_panel",
      "run_one_ears_sim_dgp2"
    )
  ) %dopar% {
    run_one_ears_sim_dgp2(
      alpha_raw = alpha_raw,
      alpha_smooth = alpha_smooth,
      scale = scale
    )
  }
}

# =========================================================
# EpiEstim helpers
# =========================================================
run_epiestim_panel <- function(series, q_probs, mean_si = 2, std_si = 1, pad_n = 7) {
  incid <- data.frame(I = series)
  
  res <- estimate_R(
    incid = incid,
    method = "parametric_si",
    config = make_config(list(
      mean_si = mean_si,
      std_si = std_si
    ))
  )
  
  qmat <- sapply(q_probs, function(p) {
    sapply(seq_len(nrow(res$R)), function(i) {
      mu <- res$R$`Mean(R)`[i]
      sigma <- res$R$`Std(R)`[i]
      shape <- (mu / sigma)^2
      rate  <- shape / mu
      qgamma(p, shape = shape, rate = rate)
    })
  })
  
  qmat <- rbind(matrix(0, nrow = pad_n, ncol = ncol(qmat)), qmat)
  qmat <- ifelse(qmat > 1, 1, 0)
  
  lapply(seq_len(ncol(qmat)), function(i) qmat[, i])
}

run_one_epi_sim_dgp2 <- function(use_smooth = FALSE,
                                 q_probs,
                                 scale = 5,
                                 mean_si = 2,
                                 std_si = 1) {
  sim <- simulate_series_dgp2(scale = scale)
  y  <- sim$raw
  y1 <- sim$smooth
  
  series <- if (use_smooth) y1 else y
  t1 <- 211
  t2 <- max(212, which.max(series[211:260]) + 210)
  
  signals <- run_epiestim_panel(
    series = series,
    q_probs = q_probs,
    mean_si = mean_si,
    std_si = std_si
  )
  
  alarm_rate <- sapply(signals, get_alarm_rate)
  timeliness <- sapply(signals, get_timeliness, t1 = t1, t2 = t2)
  
  c(alarm_rate, timeliness)
}

run_epi_simulation_dgp2 <- function(totaliter, use_smooth, q_probs, scale = 5) {
  foreach(
    jj = 1:totaliter,
    .combine = "rbind",
    .packages = c("stats", "EpiEstim"),
    .export = c(
      "simulate_series_dgp2",
      "sevenmaho",
      "get_timeliness",
      "get_alarm_rate",
      "run_epiestim_panel",
      "run_one_epi_sim_dgp2"
    )
  ) %dopar% {
    run_one_epi_sim_dgp2(
      use_smooth = use_smooth,
      q_probs = q_probs,
      scale = scale
    )
  }
}

# =========================================================
# Bootstrap helpers
# =========================================================
bootstrap_threshold <- function(H, fit, index, bound, maxiter = 500) {
  fitted_vals <- as.numeric(fit$fitted)
  fitted_vals[is.na(fitted_vals)] <- 0
  fitted_vals[fitted_vals < 0] <- 0
  
  start_idx <- index - 15
  if (start_idx < 1) {
    stop("index - 15 must be at least 1.")
  }
  
  lambda_vec <- fitted_vals[start_idx:index]
  n_lambda <- length(lambda_vec)
  
  sim_sum <- numeric(maxiter)
  
  for (b in seq_len(maxiter)) {
    sim_vals <- rpois(n_lambda, lambda = lambda_vec)
    sim_sum[b] <- sum(sim_vals[(n_lambda - H + 1):n_lambda])
  }
  
  sim_sum <- sort(sim_sum)
  upper_idx <- max(1, min(maxiter, floor(maxiter * (1 - bound))))
  
  sim_sum[upper_idx]
}

count_bootstrap_alarm <- function(series, t1, t2, K, H, bound) {
  valid_jj <- (t1:t2)[(t1:t2) >= (H + 16)]
  
  if (length(valid_jj) == 0) {
    return(0)
  }
  
  res <- foreach(
    jj = valid_jj,
    .combine = "rbind",
    .packages = c("forecast", "stats"),
    .export = c("bootstrap_threshold")
  ) %dopar% {
    index <- jj - H
    series_sub <- series[1:jj]
    train <- series_sub[1:index]
    cur_val <- sum(series_sub[(jj - H + 1):jj])
    
    fit <- auto.arima(train)
    fit$fitted[fit$fitted < 0] <- 0
    
    up <- bootstrap_threshold(H = H, fit = fit, index = index, bound = bound)
    c(jj = jj, upper = up, cur = cur_val)
  }
  
  res <- as.data.frame(res)
  res$sig <- as.integer(res$cur > res$upper)
  
  out <- 0
  for (ij in K:nrow(res)) {
    if (sum(res$sig[(ij - K + 1):ij]) == K) {
      out <- out + 1
    }
  }
  
  out
}

first_bootstrap_alarm <- function(series, t1, t2, K, H, bound) {
  valid_jj <- (t1:t2)[(t1:t2) >= (H + 16)]
  
  if (length(valid_jj) == 0) {
    return(t2)
  }
  
  res <- foreach(
    jj = valid_jj,
    .combine = "rbind",
    .packages = c("forecast", "stats"),
    .export = c("bootstrap_threshold")
  ) %dopar% {
    index <- jj - H
    series_sub <- series[1:jj]
    train <- series_sub[1:index]
    cur_val <- sum(series_sub[(jj - H + 1):jj])
    
    fit <- auto.arima(train)
    fit$fitted[fit$fitted < 0] <- 0
    
    up <- bootstrap_threshold(H = H, fit = fit, index = index, bound = bound)
    c(jj = jj, upper = up, cur = cur_val)
  }
  
  res <- as.data.frame(res)
  res$sig <- as.integer(res$cur > res$upper)
  
  out <- t2
  for (ij in K:nrow(res)) {
    if (sum(res$sig[(ij - K + 1):ij]) == K) {
      out <- res$jj[ij]
      break
    }
  }
  
  out
}

run_one_bootstrap_sim_dgp2 <- function(scale = 5,
                                       bound,
                                       use_smooth = TRUE,
                                       H = 2,
                                       K = 1) {
  sim <- simulate_series_dgp2(scale = scale)
  series <- if (use_smooth) sim$smooth else sim$raw
  
  bg_start <- max(12, H + 16)
  bg_end   <- 210
  
  n_alarm <- count_bootstrap_alarm(
    series = series,
    t1 = bg_start,
    t2 = bg_end,
    K = K,
    H = H,
    bound = bound
  )
  
  fpr <- n_alarm / length(bg_start:bg_end)
  
  out_start <- 211
  out_end   <- max(212, which.max(series[211:260]) + 210)
  
  first_alarm <- first_bootstrap_alarm(
    series = series,
    t1 = out_start,
    t2 = out_end,
    K = K,
    H = H,
    bound = bound
  )
  
  timeliness <- (out_end - first_alarm) / (out_end - out_start)
  
  c(fpr = fpr, timeliness = timeliness)
}

run_bootstrap_tuning_dgp2 <- function(totaliter,
                                      bound_res,
                                      scale = 5,
                                      use_smooth = TRUE,
                                      H = 2,
                                      K = 1) {
  fpr_vec <- numeric(length(bound_res))
  timeliness_vec <- numeric(length(bound_res))
  
  for (z in seq_along(bound_res)) {
    bound <- bound_res[z]
    
    tmp <- replicate(
      totaliter,
      run_one_bootstrap_sim_dgp2(
        scale = scale,
        bound = bound,
        use_smooth = use_smooth,
        H = H,
        K = K
      )
    )
    
    tmp <- t(tmp)
    fpr_vec[z] <- mean(tmp[, "fpr"])
    timeliness_vec[z] <- mean(tmp[, "timeliness"])
  }
  
  list(
    fpr = fpr_vec,
    timeliness = timeliness_vec
  )
}

# =========================================================
# RUN: EARS
# =========================================================
totaliter <- 1000
scale1 <- 5

# low FPR
ress1 <- run_ears_simulation_dgp2(
  totaliter = totaliter,
  alpha_raw    = c(0.00005, 0.000005, 0.00000005),
  alpha_smooth = c(0.0003, 0.000000000045, 0.00000000000000005),
  scale = scale1
)

# medium FPR
ress2 <- run_ears_simulation_dgp2(
  totaliter = totaliter,
  alpha_raw    = c(0.0002, 0.00008, 0.000005),
  alpha_smooth = c(0.0009, 0.000000025, 0.00000000000000005),
  scale = scale1
)

# high FPR
ress3 <- run_ears_simulation_dgp2(
  totaliter = totaliter,
  alpha_raw    = c(0.002, 0.001, 0.00018),
  alpha_smooth = c(0.0045, 0.00000085, 0.0000000000000005),
  scale = scale1
)

ress1 <- clean_result(ress1)
ress2 <- clean_result(ress2)
ress3 <- clean_result(ress3)

cat("\ncolMeans(ress1):\n")
print(colMeans(ress1))

cat("\ncolMeans(ress2):\n")
print(colMeans(ress2))

cat("\ncolMeans(ress3):\n")
print(colMeans(ress3))

# =========================================================
# RUN: EpiEstim
# =========================================================
ress4 <- run_epi_simulation_dgp2(
  totaliter = totaliter,
  use_smooth = FALSE,
  q_probs = seq(0.05, 0.10, by = 0.01),
  scale = scale1
)

ress5 <- run_epi_simulation_dgp2(
  totaliter = totaliter,
  use_smooth = TRUE,
  q_probs = seq(0.13, 0.18, by = 0.01),
  scale = scale1
)

ress4 <- clean_result(ress4)
ress5 <- clean_result(ress5)

cat("\ncolMeans(ress4):\n")
print(colMeans(ress4))

cat("\ncolMeans(ress5):\n")
print(colMeans(ress5))

# =========================================================
# RUN: Bootstrap tuning for model 2
# smooth and raw
# =========================================================
totaliter_boot <- 200
scale1 <- 5

bound_res1 <- seq(0.1, 0.22, by = 0.02)    # smooth
bound_res2 <- seq(0.005, 0.065, by = 0.01) # raw

# note: your original factor1=5 is only one number,
# so it effectively runs once
factor1 <- c(5)

fpr_table3 <- list()         # smooth
timeliness_table3 <- list()
fpr_table4 <- list()         # raw
timeliness_table4 <- list()

for (iter in seq_along(factor1)) {
  scale_use <- factor1[iter]
  
  boot_smooth <- run_bootstrap_tuning_dgp2(
    totaliter = totaliter_boot,
    bound_res = bound_res1,
    scale = scale_use,
    use_smooth = TRUE,
    H = 2,
    K = 1
  )
  
  boot_raw <- run_bootstrap_tuning_dgp2(
    totaliter = totaliter_boot,
    bound_res = bound_res2,
    scale = scale_use,
    use_smooth = FALSE,
    H = 2,
    K = 2
  )
  
  fpr_table3[[iter]] <- boot_smooth$fpr
  timeliness_table3[[iter]] <- boot_smooth$timeliness
  
  fpr_table4[[iter]] <- boot_raw$fpr
  timeliness_table4[[iter]] <- boot_raw$timeliness
}

cat("\nSmooth bootstrap FPR:\n")
print(fpr_table3)

cat("\nSmooth bootstrap timeliness:\n")
print(timeliness_table3)

cat("\nRaw bootstrap FPR:\n")
print(fpr_table4)

cat("\nRaw bootstrap timeliness:\n")
print(timeliness_table4)

# =========================================================
# Stop cluster
# =========================================================
stopCluster(cl)