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
# DGP for model 3
# =========================================================
simulate_series_model3 <- function(scale = 10) {
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
  
  y <- VOC <- sample_vec <- c(5, 5, 5)
  
  for (i in 4:330) {
    if (i < 201) {
      y[i]   <- y[i - 1]   * 0.55 + y[i - 2]   * 0.30 + rnorm(1, 0, 1) + 2
      VOC[i] <- VOC[i - 1] * 0.55 + VOC[i - 2] * 0.30 + rnorm(1, 0, 1) + 2
    } else if (i < 281) {
      y[i]   <- y[i - 1]   * 0.55 + y[i - 2]   * 0.30 + rnorm(1, 0, 1) + 2 * 0.96^((i - 200) / 3)
      VOC[i] <- VOC[i - 1] * 0.55 + VOC[i - 2] * 0.30 + rnorm(1, 0, 1) + 2 * 0.96^((i - 200) / 3)
    } else {
      y[i]   <- y[i - 1]   * 0.55 + y[i - 2]   * 0.30 + rnorm(1, 0, 1) + 0.6733841 * 1.05^((i - 280) / 3)
      VOC[i] <- VOC[i - 1] * 0.55 + VOC[i - 2] * 0.30 + rnorm(1, 0, 1) + 1 * 1.05^((i - 280) / 3)
    }
  }
  
  y[y < 0] <- 0.1
  VOC[VOC < 0] <- 0.1
  
  label3_sub <- label3[71:330]
  
  sample_uns <- y[71:330] * scale * 0.3
  sample_s   <- sevenmaho(y[71:330], label3_sub) * scale * 0.3
  
  for (i in 4:330) {
    if (i < 201) {
      y[i]   <- y[i]   + 2 / label[i]
      VOC[i] <- VOC[i] + 2 / label[i]
    } else if (i < 281) {
      y[i]   <- y[i]   + 4 / label[i]
      VOC[i] <- VOC[i] + 4 / label[i]
    } else {
      y[i]   <- y[i]   + 1 / label[i]
      VOC[i] <- VOC[i] + 1 / label[i]
    }
  }
  
  y_sub   <- y[71:330] * scale
  VOC_sub <- VOC[71:330] * 0.3 * scale * 0.2
  
  # non-smoothed ratio
  VOC_uns <- pmin(VOC_sub, sample_uns)
  ratio_uns <- VOC_uns / sample_uns
  y_uns_est <- y_sub * ratio_uns
  
  # smoothed ratio
  VOC_s <- sevenmaho(VOC_sub, label3_sub)
  VOC_s <- pmin(VOC_s, sample_s)
  ratio_s <- VOC_s / sample_s
  y_s_est <- sevenmaho(y_sub * ratio_s, label3_sub)
  
  list(
    y = y_sub,
    y_uns_est = y_uns_est,
    y_s_est = y_s_est,
    sample_uns = sample_uns,
    sample_s = sample_s,
    VOC_uns = VOC_uns,
    VOC_s = VOC_s,
    label3 = label3_sub
  )
}

# =========================================================
# Utility
# =========================================================
get_alarm_rate <- function(signals, bg_index = 12:210) {
  mean(signals[bg_index] == 1, na.rm = TRUE)
}

get_timeliness <- function(signals, t1, t2, detect_start = 211, detect_end = 260) {
  idx <- which(signals[detect_start:detect_end] == 1)[1]
  
  if (is.na(idx)) {
    first_alarm <- t2
  } else {
    first_alarm <- idx + detect_start - 1
  }
  
  (t2 - first_alarm) / (t2 - t1)
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

run_one_ears_model3 <- function(alpha_raw, alpha_smooth, scale = 10) {
  sim <- simulate_series_model3(scale = scale)
  
  y1 <- sim$y_uns_est
  y2 <- sim$y_s_est
  
  t1 <- 211
  t2_raw <- max(212, which.max(y1[211:260]) + 210)
  t2_s   <- max(212, which.max(y2[211:260]) + 210)
  
  sig_raw <- run_ears_panel(y1, alpha_raw)
  sig_s   <- run_ears_panel(y2, alpha_smooth)
  
  signals <- c(sig_raw, sig_s)
  
  alarm_rate <- sapply(signals, get_alarm_rate)
  timeliness <- c(
    sapply(sig_raw, get_timeliness, t1 = t1, t2 = t2_raw),
    sapply(sig_s,   get_timeliness, t1 = t1, t2 = t2_s)
  )
  
  c(alarm_rate, timeliness)
}

run_ears_simulation_model3 <- function(totaliter, alpha_raw, alpha_smooth, scale = 10) {
  foreach(
    jj = 1:totaliter,
    .combine = "rbind",
    .packages = c("stats", "surveillance"),
    .export = c(
      "sevenmaho",
      "simulate_series_model3",
      "get_alarm_rate",
      "get_timeliness",
      "run_ears_signal",
      "run_ears_panel",
      "run_one_ears_model3"
    )
  ) %dopar% {
    run_one_ears_model3(
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

run_one_epi_model3 <- function(use_smooth = FALSE, q_probs, scale = 10) {
  sim <- simulate_series_model3(scale = scale)
  
  series <- if (use_smooth) sim$y_s_est else sim$y_uns_est
  t1 <- 211
  t2 <- max(212, which.max(series[211:260]) + 210)
  
  signals <- run_epiestim_panel(
    series = series,
    q_probs = q_probs,
    mean_si = 2,
    std_si = 1
  )
  
  alarm_rate <- sapply(signals, get_alarm_rate)
  timeliness <- sapply(signals, get_timeliness, t1 = t1, t2 = t2)
  
  c(alarm_rate, timeliness)
}

run_epi_simulation_model3 <- function(totaliter, use_smooth, q_probs, scale = 10) {
  foreach(
    jj = 1:totaliter,
    .combine = "rbind",
    .packages = c("stats", "EpiEstim"),
    .export = c(
      "sevenmaho",
      "simulate_series_model3",
      "get_alarm_rate",
      "get_timeliness",
      "run_epiestim_panel",
      "run_one_epi_model3"
    )
  ) %dopar% {
    run_one_epi_model3(
      use_smooth = use_smooth,
      q_probs = q_probs,
      scale = scale
    )
  }
}

# =========================================================
# Logistic helpers
# =========================================================
build_logistic_data <- function(val1, val2) {
  val1[val1 < 1] <- 1
  val2[val2 < 1] <- 1
  
  time_vec <- integer(0)
  class_vec <- integer(0)
  
  for (ij in seq_along(val1)) {
    time_vec  <- c(time_vec, rep(ij, val1[ij]), rep(ij, val2[ij]))
    class_vec <- c(class_vec, rep(1, val1[ij]), rep(0, val2[ij]))
  }
  
  data.frame(time = time_vec, class = class_vec)
}

bootstrap_logi_count <- function(t1, t2, val1t, val2t, alpha1) {
  ress1 <- foreach(
    rs = t1:t2,
    .combine = "rbind",
    .packages = c("stats"),
    .export = c("build_logistic_data")
  ) %dopar% {
    val1 <- val1t[1:rs]
    val2 <- val2t[1:rs]
    
    dat <- build_logistic_data(val1, val2)
    fitl <- glm(class ~ time, data = dat, family = "binomial")
    
    coef_adj <- fitl$coefficients - alpha1 * sqrt(diag(vcov(fitl)))
    slope <- coef_adj[2]
    if (is.na(slope)) slope <- -1
    
    c(as.integer(slope > 0))
  }
  
  sum(ress1[, 1])
}

bootstrap_logi_first <- function(t1, t2, val1t, val2t, alpha1) {
  ress1 <- foreach(
    rs = t1:t2,
    .combine = "rbind",
    .packages = c("stats"),
    .export = c("build_logistic_data")
  ) %dopar% {
    val1 <- val1t[1:rs]
    val2 <- val2t[1:rs]
    
    dat <- build_logistic_data(val1, val2)
    fitl <- glm(class ~ time, data = dat, family = "binomial")
    
    coef_adj <- fitl$coefficients - alpha1 * sqrt(diag(vcov(fitl)))
    slope <- coef_adj[2]
    if (is.na(slope)) slope <- -1
    
    c(rs, as.integer(slope > 0))
  }
  
  output <- t2
  for (iq in seq_len(nrow(ress1))) {
    if (ress1[iq, 2] == 1) {
      output <- ress1[iq, 1]
      break
    }
  }
  
  output
}

run_logistic_tuning_model3 <- function(totaliter = 300,
                                       scale = 10,
                                       alpha_raw = seq(4, 5, by = 0.15),
                                       alpha_smooth = seq(4, 5, by = 0.15)) {
  fpr_raw <- numeric(length(alpha_raw))
  time_raw <- numeric(length(alpha_raw))
  fpr_s <- numeric(length(alpha_smooth))
  time_s <- numeric(length(alpha_smooth))
  
  for (jjj in seq_along(alpha_raw)) {
    a1 <- alpha_raw[jjj]
    a2 <- alpha_smooth[jjj]
    
    fpr1 <- numeric(totaliter)
    timeliness1 <- numeric(totaliter)
    fpr2 <- numeric(totaliter)
    timeliness2 <- numeric(totaliter)
    
    for (iter in seq_len(totaliter)) {
      sim <- simulate_series_model3(scale = scale)
      
      # non-smoothed
      val1t <- sim$VOC_uns
      val2t <- sim$sample_uns - val1t
      
      out_fpr <- bootstrap_logi_count(12, 210, val1t, val2t, alpha1 = a1)
      fpr1[iter] <- out_fpr / (210 - 12 + 1)
      
      t1 <- 211
      t2 <- max(212, which.max(val1t[211:260]) + 210)
      out_time <- bootstrap_logi_first(t1, t2, val1t, val2t, alpha1 = a1)
      timeliness1[iter] <- (t2 - out_time) / (t2 - t1)
      
      # smoothed
      val1t_s <- sim$VOC_s
      val2t_s <- sim$sample_s - val1t_s
      
      out_fpr_s <- bootstrap_logi_count(12, 210, val1t_s, val2t_s, alpha1 = a2)
      fpr2[iter] <- out_fpr_s / (210 - 12 + 1)
      
      t2s <- max(212, which.max(val1t_s[211:260]) + 210)
      out_time_s <- bootstrap_logi_first(t1, t2s, val1t_s, val2t_s, alpha1 = a2)
      timeliness2[iter] <- (t2s - out_time_s) / (t2s - t1)
    }
    
    fpr_raw[jjj] <- mean(fpr1)
    time_raw[jjj] <- mean(timeliness1)
    fpr_s[jjj] <- mean(fpr2)
    time_s[jjj] <- mean(timeliness2)
  }
  
  list(
    fpr_raw = fpr_raw,
    timeliness_raw = time_raw,
    fpr_smooth = fpr_s,
    timeliness_smooth = time_s
  )
}

# =========================================================
# Bootstrap helpers for model 3
# =========================================================
bootstrap_threshold_model3 <- function(H, fit1, fit2, sample_size, index, bound, maxiter = 400) {
  start_idx <- index - 15
  if (start_idx < 1) stop("index - 15 must be at least 1.")
  
  fit1_vals <- as.numeric(fit1$fitted)
  fit2_vals <- as.numeric(fit2$fitted)
  
  fit1_vals[is.na(fit1_vals)] <- 0
  fit2_vals[is.na(fit2_vals)] <- 0
  fit1_vals[fit1_vals < 0] <- 0
  fit2_vals[fit2_vals < 0] <- 0
  
  sim_sum <- numeric(maxiter)
  
  for (b in seq_len(maxiter)) {
    out <- numeric(index - start_idx + 1)
    pos <- 1
    
    for (number in start_idx:index) {
      out[pos] <- rpois(1, fit1_vals[number]) * rpois(1, fit2_vals[number]) / sample_size[number]
      pos <- pos + 1
    }
    
    out[is.na(out)] <- 0
    out <- round(out)
    sim_sum[b] <- sum(out[(length(out) - H + 1):length(out)])
  }
  
  sim_sum <- sort(sim_sum)
  upper_idx <- max(1, min(maxiter, floor(maxiter * (1 - bound))))
  lower_idx <- max(1, min(maxiter, floor(maxiter * 0.05)))
  
  list(
    lower = sim_sum[lower_idx],
    upper = sim_sum[upper_idx]
  )
}

count_bootstrap_alarm_model3 <- function(series, VOC, sample, t1, t2, K, H, bound) {
  valid_jj <- (t1:t2)[(t1:t2) >= (H + 16)]
  
  if (length(valid_jj) == 0) return(0)
  
  res <- foreach(
    jj = valid_jj,
    .combine = "rbind",
    .packages = c("forecast", "stats"),
    .export = c("bootstrap_threshold_model3")
  ) %dopar% {
    index <- jj - H
    
    series_train <- series[1:index]
    VOC_train <- VOC[1:index]
    sample_train <- sample[1:index]
    
    fit1 <- auto.arima(series_train)
    fit2 <- auto.arima(VOC_train)
    fit1$fitted[fit1$fitted < 0] <- 0
    fit2$fitted[fit2$fitted < 0] <- 0
    
    est <- series[1:jj] * VOC[1:jj] / sample[1:jj]
    cur_val <- sum(est[(jj - H + 1):jj])
    
    bt <- bootstrap_threshold_model3(H, fit1, fit2, sample_train, index, bound)
    c(jj, bt$upper, cur_val)
  }
  
  res <- as.data.frame(res)
  res$sig <- as.integer(res[, 3] > res[, 2])
  
  out <- 0
  for (ij in K:nrow(res)) {
    if (sum(res$sig[(ij - K + 1):ij]) == K) {
      out <- out + 1
    }
  }
  
  out
}

first_bootstrap_alarm_model3 <- function(series, VOC, sample, t1, t2, K, H, bound) {
  valid_jj <- (t1:t2)[(t1:t2) >= (H + 16)]
  
  if (length(valid_jj) == 0) return(t2)
  
  res <- foreach(
    jj = valid_jj,
    .combine = "rbind",
    .packages = c("forecast", "stats"),
    .export = c("bootstrap_threshold_model3")
  ) %dopar% {
    index <- jj - H
    
    series_train <- series[1:index]
    VOC_train <- VOC[1:index]
    sample_train <- sample[1:index]
    
    fit1 <- auto.arima(series_train)
    fit2 <- auto.arima(VOC_train)
    fit1$fitted[fit1$fitted < 0] <- 0
    fit2$fitted[fit2$fitted < 0] <- 0
    
    est <- series[1:jj] * VOC[1:jj] / sample[1:jj]
    cur_val <- sum(est[(jj - H + 1):jj])
    
    bt <- bootstrap_threshold_model3(H, fit1, fit2, sample_train, index, bound)
    c(jj, bt$upper, cur_val)
  }
  
  res <- as.data.frame(res)
  res$sig <- as.integer(res[, 3] > res[, 2])
  
  out <- t2
  for (ij in K:nrow(res)) {
    if (sum(res$sig[(ij - K + 1):ij]) == K) {
      out <- res[ij, 1]
      break
    }
  }
  
  out
}

run_bootstrap_tuning_model3 <- function(totaliter = 50,
                                        factor1 = c(10),
                                        bound_res_s = seq(0.31, 0.37, by = 0.01),
                                        bound_res_u = seq(0.02, 0.14, by = 0.02)) {
  fpr_table_s <- list()
  timeliness_table_s <- list()
  fpr_table_u <- list()
  timeliness_table_u <- list()
  
  for (iter in seq_along(factor1)) {
    scale_use <- factor1[iter]
    
    fpr_s <- numeric(length(bound_res_s))
    time_s <- numeric(length(bound_res_s))
    fpr_u <- numeric(length(bound_res_u))
    time_u <- numeric(length(bound_res_u))
    
    for (zzz in seq_along(bound_res_s)) {
      bound1 <- bound_res_s[zzz]
      bound2 <- bound_res_u[zzz]
      
      fpr1 <- numeric(totaliter)
      timeliness1 <- numeric(totaliter)
      fpr2 <- numeric(totaliter)
      timeliness2 <- numeric(totaliter)
      
      for (jj in seq_len(totaliter)) {
        sim <- simulate_series_model3(scale = scale_use)
        
        # unsmoothed
        out_fpr_u <- count_bootstrap_alarm_model3(
          series = sim$y,
          VOC = sim$VOC_uns,
          sample = sim$sample_uns,
          t1 = 12,
          t2 = 210,
          K = 1,
          H = 2,
          bound = bound2
        )
        fpr1[jj] <- out_fpr_u / length(12:210)
        
        t2_u <- max(212, which.max(sim$y[211:260]) + 210)
        out_time_u <- first_bootstrap_alarm_model3(
          series = sim$y,
          VOC = sim$VOC_uns,
          sample = sim$sample_uns,
          t1 = 211,
          t2 = t2_u,
          K = 1,
          H = 2,
          bound = bound2
        )
        timeliness1[jj] <- (t2_u - out_time_u) / (t2_u - 211)
        
        # smoothed
        y1 <- sevenmaho(sim$y, sim$label3)
        out_fpr_s <- count_bootstrap_alarm_model3(
          series = y1,
          VOC = sim$VOC_s,
          sample = sim$sample_s,
          t1 = 12,
          t2 = 210,
          K = 1,
          H = 2,
          bound = bound1
        )
        fpr2[jj] <- out_fpr_s / length(12:210)
        
        t2_s <- max(212, which.max(y1[211:260]) + 210)
        out_time_s <- first_bootstrap_alarm_model3(
          series = y1,
          VOC = sim$VOC_s,
          sample = sim$sample_s,
          t1 = 211,
          t2 = t2_s,
          K = 1,
          H = 2,
          bound = bound1
        )
        timeliness2[jj] <- (t2_s - out_time_s) / (t2_s - 211)
      }
      
      fpr_s[zzz] <- mean(fpr2)
      time_s[zzz] <- mean(timeliness2)
      fpr_u[zzz] <- mean(fpr1)
      time_u[zzz] <- mean(timeliness1)
    }
    
    fpr_table_s[[iter]] <- fpr_s
    timeliness_table_s[[iter]] <- time_s
    fpr_table_u[[iter]] <- fpr_u
    timeliness_table_u[[iter]] <- time_u
  }
  
  list(
    fpr_table_s = fpr_table_s,
    timeliness_table_s = timeliness_table_s,
    fpr_table_u = fpr_table_u,
    timeliness_table_u = timeliness_table_u
  )
}

# =========================================================
# Run EARS
# =========================================================
totaliter <- 1000
scale1 <- 10

ress1 <- run_ears_simulation_model3(
  totaliter = totaliter,
  alpha_raw    = c(0.00007, 0.00015, 0.00005),
  alpha_smooth = c(0.00019, 0.000000000002, 0.00000000000000008),
  scale = scale1
)

ress2 <- run_ears_simulation_model3(
  totaliter = totaliter,
  alpha_raw    = c(0.001, 0.001, 0.0005),
  alpha_smooth = c(0.0006, 0.0000000003, 0.00000000000000008),
  scale = scale1
)

ress3 <- run_ears_simulation_model3(
  totaliter = totaliter,
  alpha_raw    = c(0.0075, 0.006, 0.005),
  alpha_smooth = c(0.002, 0.00000008, 0.00000000000000008),
  scale = scale1
)

ress1 <- clean_result(ress1)
ress2 <- clean_result(ress2)
ress3 <- clean_result(ress3)

colMeans(ress1)
colMeans(ress2)
colMeans(ress3)

# =========================================================
# Run EpiEstim
# =========================================================
ress4 <- run_epi_simulation_model3(
  totaliter = totaliter,
  use_smooth = FALSE,
  q_probs = seq(0.15, 0.21, by = 0.01),
  scale = scale1
)

ress5 <- run_epi_simulation_model3(
  totaliter = totaliter,
  use_smooth = TRUE,
  q_probs = seq(0.22, 0.27, by = 0.01),
  scale = scale1
)

ress4 <- clean_result(ress4)
ress5 <- clean_result(ress5)

colMeans(ress4)
colMeans(ress5)

# =========================================================
# Run logistic tuning
# =========================================================
logi_out <- run_logistic_tuning_model3(
  totaliter = 500,
  scale = 10,
  alpha_raw = seq(4, 5, by = 0.15),
  alpha_smooth = seq(4, 5, by = 0.15)
)

fpr1z <- logi_out$fpr_raw
timeliness1z <- logi_out$timeliness_raw
fpr2z <- logi_out$fpr_smooth
timeliness2z <- logi_out$timeliness_smooth

# =========================================================
# Run bootstrap tuning
# =========================================================
boot_out <- run_bootstrap_tuning_model3(
  totaliter = 500,
  factor1 = c(10),
  bound_res_s = seq(0.31, 0.37, by = 0.01),
  bound_res_u = seq(0.02, 0.14, by = 0.02)
)

fpr_table5 <- boot_out$fpr_table_s
timeliness_table5 <- boot_out$timeliness_table_s
fpr_table6 <- boot_out$fpr_table_u
timeliness_table6 <- boot_out$timeliness_table_u

# =========================================================
# Stop cluster
# =========================================================
stopCluster(cl)