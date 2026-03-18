library(foreach)
library(doParallel)
library(surveillance)
library(EpiEstim)
library(forecast)

# =========================================================
# Parallel setup
# =========================================================
num_cores <- parallel::detectCores()
cl <- parallel::makeCluster(num_cores)
doParallel::registerDoParallel(cl)

# =========================================================
# Holiday-adjusted 7-day moving average
# label == 0: non-working day
# label != 0: working day
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
# Simulate one epidemic series
# =========================================================
simulate_series <- function(scale = 40) {
  label_pattern <- c(1, 1, 1, 1, 1, 0, 0)
  label <- rep(label_pattern, 70)
  
  y <- c(5, 5, 5)
  for (i in 4:330) {
    drift <- if (i < 281) 2 else 2.5
    y[i] <- y[i - 1] * 0.55 + y[i - 2] * 0.30 + rnorm(1, mean = 0, sd = 0.5) + drift
  }
  
  y <- y[71:330] * scale
  label <- label[71:330]
  y_smooth <- sevenmaho(y, label)
  
  list(
    raw = y,
    smooth = y_smooth,
    label = label
  )
}

# =========================================================
# Convert first signal in detection window to timeliness
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

# =========================================================
# Background alarm rate
# =========================================================
get_alarm_rate <- function(signals, bg_index = 12:210) {
  mean(signals[bg_index] == 1, na.rm = TRUE)
}

# =========================================================
# Run one EARS method
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

# =========================================================
# Run multiple EARS methods for one series
# =========================================================
run_ears_panel <- function(series, alpha_vec, baseline = 7) {
  methods <- c("C1", "C2", "C3")
  lapply(seq_along(methods), function(i) {
    run_ears_signal(
      series   = series,
      method   = methods[i],
      alpha    = alpha_vec[i],
      baseline = baseline
    )
  })
}

# =========================================================
# Run EpiEstim and generate binary alarm signals
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

# =========================================================
# One simulation run for EARS
# =========================================================
run_one_ears_sim <- function(alpha_raw, alpha_smooth, scale = 40) {
  sim <- simulate_series(scale = scale)
  y  <- sim$raw
  y1 <- sim$smooth
  
  t1 <- 211
  t2_raw <- max(212, which.max(y[211:260]) + 210)
  t2_s   <- max(212, which.max(y1[211:260]) + 210)
  
  sig_raw <- run_ears_panel(y,  alpha_vec = alpha_raw)
  sig_s   <- run_ears_panel(y1, alpha_vec = alpha_smooth)
  
  signals <- c(sig_raw, sig_s)
  
  alarm_rate <- sapply(signals, get_alarm_rate)
  
  timeliness <- c(
    sapply(sig_raw, get_timeliness, t1 = t1, t2 = t2_raw),
    sapply(sig_s,   get_timeliness, t1 = t1, t2 = t2_s)
  )
  
  c(alarm_rate, timeliness)
}

# =========================================================
# Repeated EARS simulation
# =========================================================
run_ears_simulation <- function(totaliter, alpha_raw, alpha_smooth, scale = 40) {
  foreach(
    jj = 1:totaliter,
    .combine = "rbind",
    .packages = c("forecast", "stats", "surveillance"),
    .export = c(
      "simulate_series",
      "sevenmaho",
      "get_timeliness",
      "get_alarm_rate",
      "run_ears_signal",
      "run_ears_panel",
      "run_one_ears_sim"
    )
  ) %dopar% {
    run_one_ears_sim(
      alpha_raw = alpha_raw,
      alpha_smooth = alpha_smooth,
      scale = scale
    )
  }
}

# =========================================================
# One simulation run for EpiEstim
# =========================================================
run_one_epi_sim <- function(use_smooth = FALSE,
                            q_probs,
                            scale = 40,
                            mean_si = 2,
                            std_si = 1) {
  sim <- simulate_series(scale = scale)
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

# =========================================================
# Repeated EpiEstim simulation
# =========================================================
run_epi_simulation <- function(totaliter, use_smooth, q_probs, scale = 40) {
  foreach(
    jj = 1:totaliter,
    .combine = "rbind",
    .packages = c("forecast", "stats", "EpiEstim"),
    .export = c(
      "simulate_series",
      "sevenmaho",
      "get_timeliness",
      "get_alarm_rate",
      "run_epiestim_panel",
      "run_one_epi_sim"
    )
  ) %dopar% {
    run_one_epi_sim(
      use_smooth = use_smooth,
      q_probs = q_probs,
      scale = scale
    )
  }
}

# =========================================================
# Clean result matrix
# =========================================================
clean_result <- function(x) {
  x[x < 0] <- 0
  x[is.na(x)] <- 0
  x
}

# =========================================================
# Bootstrap threshold from fitted ARIMA
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

# =========================================================
# Count alarms in a window
# =========================================================
count_bootstrap_alarm <- function(series, t1, t2, K, H, bound) {
  res <- foreach(
    jj = t1:t2,
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

# =========================================================
# First alarm time in a window
# =========================================================
first_bootstrap_alarm <- function(series, t1, t2, K, H, bound) {
  res <- foreach(
    jj = t1:t2,
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

# =========================================================
# One simulation for bootstrap tuning
# =========================================================
run_one_bootstrap_sim <- function(scale = 40, bound, use_smooth = TRUE, H = 2, K = 1) {
  sim <- simulate_series(scale = scale)
  series <- if (use_smooth) sim$smooth else sim$raw
  
  # background window
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
  
  # outbreak window
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

# =========================================================
# Bootstrap tuning over bound grid
# =========================================================
run_bootstrap_tuning <- function(totaliter,
                                 bound_res,
                                 scale = 40,
                                 use_smooth = TRUE,
                                 H = 2,
                                 K = 1) {
  
  fpr_vec <- numeric(length(bound_res))
  timeliness_vec <- numeric(length(bound_res))
  
  for (z in seq_along(bound_res)) {
    bound <- bound_res[z]
    
    tmp <- replicate(
      totaliter,
      run_one_bootstrap_sim(
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
# Run all simulations
# =========================================================

# -------------------------
# EARS settings
# -------------------------
totaliter_ears <- 1000

# low FPR setting
ress1 <- run_ears_simulation(
  totaliter = totaliter_ears,
  alpha_raw    = c(0.0008, 0.000025, 0.00000005),
  alpha_smooth = c(0.0002, 0.000000000001, 0.00000000000000006),
  scale = 40
)

# medium FPR setting
ress2 <- run_ears_simulation(
  totaliter = totaliter_ears,
  alpha_raw    = c(0.0025, 0.00015, 0.000002),
  alpha_smooth = c(0.0006, 0.0000000004, 0.01),
  scale = 40
)

# high FPR setting
ress3 <- run_ears_simulation(
  totaliter = totaliter_ears,
  alpha_raw    = c(0.0075, 0.001, 0.00012),
  alpha_smooth = c(0.002, 0.00000003, 0.00000000000000006),
  scale = 40
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

# -------------------------
# EpiEstim settings
# -------------------------
totaliter_epi <- 1000

# EPI on non-smoothed
ress4 <- run_epi_simulation(
  totaliter = totaliter_epi,
  use_smooth = FALSE,
  q_probs = seq(0.015, 0.06, by = 0.008),
  scale = 40
)

# EPI on smoothed
ress5 <- run_epi_simulation(
  totaliter = totaliter_epi,
  use_smooth = TRUE,
  q_probs = seq(0.04, 0.10, by = 0.01),
  scale = 40
)

ress4 <- clean_result(ress4)
ress5 <- clean_result(ress5)

cat("\ncolMeans(ress4):\n")
print(colMeans(ress4))

cat("\ncolMeans(ress5):\n")
print(colMeans(ress5))

# -------------------------
# Bootstrap tuning
# -------------------------
totaliter_boot <- 300
scale1 <- 40

# MAH
bound_res1 <- seq(0.06, 0.14, by = 0.015)
boot_mah <- run_bootstrap_tuning(
  totaliter = totaliter_boot,
  bound_res = bound_res1,
  scale = scale1,
  use_smooth = TRUE,
  H = 2,
  K = 1
)

fpr_table1 <- list(boot_mah$fpr)
timeliness_table1 <- list(boot_mah$timeliness)

cat("\nMAH bootstrap FPR:\n")
print(fpr_table1)

cat("\nMAH bootstrap timeliness:\n")
print(timeliness_table1)

# raw/original
bound_res2 <- seq(0.0005, 0.003, by = 0.0005)
boot_raw <- run_bootstrap_tuning(
  totaliter = totaliter_boot,
  bound_res = bound_res2,
  scale = scale1,
  use_smooth = FALSE,
  H = 2,
  K = 1
)

fpr_table2 <- list(boot_raw$fpr)
timeliness_table2 <- list(boot_raw$timeliness)

cat("\nRaw bootstrap FPR:\n")
print(fpr_table2)

cat("\nRaw bootstrap timeliness:\n")
print(timeliness_table2)

# =========================================================
# Stop cluster when done
# =========================================================
stopCluster(cl)