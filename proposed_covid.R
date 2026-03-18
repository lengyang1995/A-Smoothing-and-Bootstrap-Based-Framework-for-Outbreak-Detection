library(parallel)
library(doParallel)
library(forecast)
library(foreach)

# ==================================================
# Parallel setup
# ==================================================
num_cores <- parallel::detectCores()
cl <- parallel::makeCluster(num_cores)
doParallel::registerDoParallel(cl)

# ==================================================
# Bootstrap upper/lower bound from fitted ARIMA values
# ==================================================
bootstrap_test <- function(H, fit, index,
                           maxiter = 500,
                           lower_prob = 0.08,
                           upper_prob = 0.97) {
  
  fitted_vals <- as.numeric(fit$fitted)
  fitted_vals[is.na(fitted_vals)] <- 0
  fitted_vals[fitted_vals < 0] <- 0
  
  start_idx <- index - 15
  if (start_idx < 1) {
    stop("index - 15 must be at least 1.")
  }
  
  lambda_vec <- fitted_vals[start_idx:index]
  n_lambda <- length(lambda_vec)
  
  rarc <- numeric(maxiter)
  
  for (iter in seq_len(maxiter)) {
    sim_vals <- rpois(n_lambda, lambda = lambda_vec)
    rarc[iter] <- sum(sim_vals[(n_lambda - H + 1):n_lambda])
  }
  
  rarc <- sort(rarc)
  
  lower_idx <- max(1, floor(maxiter * lower_prob))
  upper_idx <- min(maxiter, ceiling(maxiter * upper_prob))
  
  list(
    lower = rarc[lower_idx],
    upper = rarc[upper_idx]
  )
}

# ==================================================
# Generic detection function
# ==================================================
bootstrap_detect <- function(t1, t2, K, H, series_vec,
                             maxiter = 500,
                             lower_prob = 0.08,
                             upper_prob = 0.97) {
  
  ress1 <- foreach(
    jj = t1:t2,
    .combine = "rbind",
    .packages = c("forecast", "stats"),
    .export = c("bootstrap_test")
  ) %dopar% {
    
    index <- jj - H
    series_sub <- series_vec[1:jj]
    series_train <- series_sub[1:index]
    cur_val <- sum(series_sub[(jj - H + 1):jj])
    
    fit <- auto.arima(series_train)
    
    bt <- bootstrap_test(
      H = H,
      fit = fit,
      index = index,
      maxiter = maxiter,
      lower_prob = lower_prob,
      upper_prob = upper_prob
    )
    
    c(jj = jj, upper = bt$upper, cur_val = cur_val)
  }
  
  ress1 <- as.data.frame(ress1)
  ress1$sig <- as.integer(ress1$cur_val > ress1$upper)
  
  output <- t2
  
  for (ij in K:nrow(ress1)) {
    if (sum(ress1$sig[(ij - K + 1):ij]) == K) {
      output <- ress1$jj[ij]
      break
    }
  }
  
  output
}

# ==================================================
# Run one full set for a given H
# ==================================================
run_detection_for_H <- function(H, case, ma_all, mah_all, maxiter = 500) {
  
  # raw
  t1_raw <- c(34, 103, 151, 246, 399, 527)
  t2_raw <- c(45, 110, 209, 327, 468, 565)
  
  # smoothed
  t1_smooth <- c(34, 104, 153, 246, 399, 527)
  t2_smooth <- c(50, 115, 211, 331, 474, 565)
  
  pred_mat <- matrix(NA_real_, nrow = 4, ncol = 18)
  
  if (H == 2) {
    upper_prob_use <- 0.97
  } else {
    upper_prob_use <- 0.99
  }
  
  # ---------------------------
  # 1) raw series
  # ---------------------------
  raw_series <- case$Case
  
  for (K in 1:4) {
    time_or <- numeric(length(t2_raw))
    
    for (ii in seq_along(t2_raw)) {
      fit1 <- bootstrap_detect(
        t1 = t1_raw[ii],
        t2 = t2_raw[ii],
        K = K,
        H = H,
        series_vec = raw_series,
        maxiter = maxiter,
        lower_prob = 0.08,
        upper_prob = upper_prob_use
      )
      
      time_or[ii] <- (t2_raw[ii] - fit1) / (t2_raw[ii] - t1_raw[ii])
    }
    
    pred_mat[K, 1:6] <- time_or
  }
  
  # ---------------------------
  # 2) MAH series
  # ---------------------------
  for (K in 1:4) {
    time_or <- numeric(length(t2_smooth))
    
    for (ii in seq_along(t2_smooth)) {
      fit1 <- bootstrap_detect(
        t1 = t1_smooth[ii],
        t2 = t2_smooth[ii],
        K = K,
        H = H,
        series_vec = mah_all,
        maxiter = maxiter,
        lower_prob = 0.08,
        upper_prob = upper_prob_use
      )
      
      time_or[ii] <- (t2_smooth[ii] - fit1) / (t2_smooth[ii] - t1_smooth[ii])
    }
    
    pred_mat[K, 7:12] <- time_or
  }
  
  # ---------------------------
  # 3) MA series
  # ---------------------------
  for (K in 1:4) {
    time_or <- numeric(length(t2_smooth))
    
    for (ii in seq_along(t2_smooth)) {
      fit1 <- bootstrap_detect(
        t1 = t1_smooth[ii],
        t2 = t2_smooth[ii],
        K = K,
        H = H,
        series_vec = ma_all,
        maxiter = maxiter,
        lower_prob = 0.08,
        upper_prob = upper_prob_use
      )
      
      time_or[ii] <- (t2_smooth[ii] - fit1) / (t2_smooth[ii] - t1_smooth[ii])
    }
    
    pred_mat[K, 13:18] <- time_or
  }
  
  pred_mat
}

# ==================================================
# Run for H = 2, 3, 4
# ==================================================
pred_list2 <- run_detection_for_H(
  H = 2,
  case = case,
  ma_all = ma_all,
  mah_all = mah_all,
  maxiter = 500
)

pred_list3 <- run_detection_for_H(
  H = 3,
  case = case,
  ma_all = ma_all,
  mah_all = mah_all,
  maxiter = 500
)

pred_list4 <- run_detection_for_H(
  H = 4,
  case = case,
  ma_all = ma_all,
  mah_all = mah_all,
  maxiter = 500
)

# ==================================================
# Summary results
# ==================================================
summary_H2 <- c(
  mean(pred_list2[3, 1:6]),
  mean(pred_list2[2, 7:12]),
  mean(pred_list2[2, 13:18])
)

summary_H3 <- c(
  mean(pred_list3[3, 1:6]),
  mean(pred_list3[1, 7:12]),
  mean(pred_list3[1, 13:18])
)

summary_H4 <- c(
  mean(pred_list4[3, 1:6]),
  mean(pred_list4[1, 7:12]),
  mean(pred_list4[1, 13:18])
)

summary_H2
summary_H3
summary_H4

# ==================================================
# Stop cluster when done
# ==================================================
stopCluster(cl)