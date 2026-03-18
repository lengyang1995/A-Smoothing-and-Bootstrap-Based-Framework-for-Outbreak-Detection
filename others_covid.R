library(surveillance)
library(EpiEstim)

# ==================================================
# Common settings
# ==================================================
dates <- seq.Date(
  from = as.Date("2021-04-01"),
  to   = as.Date("2023-02-12"),
  by   = "day"
)

background_index <- c(
  16:27, 57:70, 76:100, 120:144,
  219:245, 337:393, 478:525, 575:640
)

start_ns <- c(34, 103, 151, 246, 399, 527)
end_ns   <- c(45, 110, 209, 327, 468, 565)

start_s  <- c(34, 104, 153, 246, 399, 527)
end_s    <- c(50, 115, 211, 331, 474, 565)

# ==================================================
# Helper: false positive rate on selected index
# ==================================================
calc_hit_rate <- function(signals, index_vec) {
  mean(signals[index_vec] == 1)
}

# ==================================================
# Helper: timeliness for outbreak windows
# ==================================================
calc_timeliness <- function(signals, dat, start_idx, end_idx) {
  signal_idx <- which(signals == 1)
  dat_signal <- dat[signal_idx, , drop = FALSE]
  
  output <- numeric(length(start_idx))
  
  for (i in seq_along(start_idx)) {
    date_start <- dat$date[start_idx[i]]
    date_end   <- dat$date[end_idx[i]]
    
    val <- 0
    
    if (nrow(dat_signal) > 0) {
      for (j in seq_len(nrow(dat_signal))) {
        if (dat_signal$date[j] >= date_start && dat_signal$date[j] <= date_end) {
          val <- which(dat$date == dat_signal$date[j])[1]
          break
        }
      }
    }
    
    if (val == 0) {
      val <- end_idx[i]
    }
    
    output[i] <- (end_idx[i] - val) / (end_idx[i] - start_idx[i])
  }
  
  c(output, mean(output))
}

# ==================================================
# Helper: run EARS
# ==================================================
run_ears <- function(series,
                     method = c("C1", "C2", "C3"),
                     alpha,
                     baseline = 7,
                     dates,
                     background_index,
                     start_idx,
                     end_idx) {
  
  method <- match.arg(method)
  dat <- data.frame(date = dates, local1 = series)
  
  offset <- switch(
    method,
    C1 = 1,
    C2 = 3,
    C3 = 5
  )
  
  pad <- switch(
    method,
    C1 = baseline,
    C2 = baseline + 2,
    C3 = baseline + 4
  )
  
  range_idx <- (baseline + offset):length(series)
  
  sts_obj <- sts(observed = series, frequency = 365)
  control <- list(
    range    = range_idx,
    method   = method,
    baseline = baseline,
    alpha    = alpha
  )
  
  surv_obj <- earsC(sts_obj, control = control)
  signals <- c(rep(0, pad), alarms(surv_obj)[, 1] * 1)
  signals[is.na(signals)] <- 0
  
  hit_rate <- calc_hit_rate(signals, background_index)
  timeliness <- calc_timeliness(signals, dat, start_idx, end_idx)
  
  list(
    signals    = signals,
    hit_rate   = hit_rate,
    timeliness = timeliness
  )
}

# ==================================================
# Helper: run EpiEstim and convert posterior quantiles to signals
# ==================================================
run_epiestim <- function(series,
                         probs,
                         dates,
                         background_index,
                         start_idx,
                         end_idx,
                         mean_si = 3,
                         std_si = 1.4,
                         threshold = 1,
                         pad_n = 7) {
  
  dat <- data.frame(date = dates, I = series)
  
  res <- estimate_R(
    incid  = dat,
    method = "parametric_si",
    config = make_config(list(
      mean_si = mean_si,
      std_si  = std_si
    ))
  )
  
  quantile_mat <- sapply(probs, function(p) {
    sapply(seq_len(nrow(res$R)), function(i) {
      mu <- res$R$`Mean(R)`[i]
      sigma <- res$R$`Std(R)`[i]
      
      shape <- (mu / sigma)^2
      rate  <- shape / mu
      
      qgamma(p, shape = shape, rate = rate)
    })
  })
  
  quantile_mat <- rbind(matrix(0, nrow = pad_n, ncol = ncol(quantile_mat)), quantile_mat)
  signal_mat <- ifelse(quantile_mat > threshold, 1, 0)
  
  fpr <- numeric(ncol(signal_mat))
  timeliness <- vector("list", ncol(signal_mat))
  
  for (k in seq_len(ncol(signal_mat))) {
    signals <- signal_mat[, k]
    fpr[k] <- calc_hit_rate(signals, background_index)
    timeliness[[k]] <- calc_timeliness(signals, dat, start_idx, end_idx)
  }
  
  list(
    res        = res,
    signal_mat = signal_mat,
    fpr        = fpr,
    timeliness = timeliness
  )
}

# ==================================================
# 1) EARS on non-smoothed series
# ==================================================
ec1_ns <- run_ears(
  series = case$Case,
  method = "C1",
  alpha = 0.03,
  baseline = 7,
  dates = dates,
  background_index = background_index,
  start_idx = start_ns,
  end_idx = end_ns
)

ec2_ns <- run_ears(
  series = case$Case,
  method = "C2",
  alpha = 0.001,
  baseline = 7,
  dates = dates,
  background_index = background_index,
  start_idx = start_ns,
  end_idx = end_ns
)

ec3_ns <- run_ears(
  series = case$Case,
  method = "C3",
  alpha = 1e-9,
  baseline = 7,
  dates = dates,
  background_index = background_index,
  start_idx = start_ns,
  end_idx = end_ns
)

ec1ns <- ec1_ns$timeliness[1:6]
ec2ns <- ec2_ns$timeliness[1:6]
ec3ns <- ec3_ns$timeliness[1:6]

ec1_ns$hit_rate
ec2_ns$hit_rate
ec3_ns$hit_rate

ec1_ns$timeliness
ec2_ns$timeliness
ec3_ns$timeliness

# ==================================================
# 2) EARS on smoothed series
# ==================================================
ec1_s <- run_ears(
  series = mah_all,
  method = "C1",
  alpha = 0.0025,
  baseline = 7,
  dates = dates,
  background_index = background_index,
  start_idx = start_s,
  end_idx = end_s
)

ec2_s <- run_ears(
  series = mah_all,
  method = "C2",
  alpha = 1e-6,
  baseline = 7,
  dates = dates,
  background_index = background_index,
  start_idx = start_s,
  end_idx = end_s
)

ec3_s <- run_ears(
  series = mah_all,
  method = "C3",
  alpha = 1e-15,
  baseline = 7,
  dates = dates,
  background_index = background_index,
  start_idx = start_s,
  end_idx = end_s
)

ec1s <- ec1_s$timeliness[1:6]
ec2s <- ec2_s$timeliness[1:6]
ec3s <- ec3_s$timeliness[1:6]

ec1_s$hit_rate
ec2_s$hit_rate
ec3_s$hit_rate

ec1_s$timeliness
ec2_s$timeliness
ec3_s$timeliness

# ==================================================
# 3) Combine EARS results
# ==================================================
ears_timeliness <- cbind(ec1ns, ec2ns, ec3ns, ec1s, ec2s, ec3s)
colMeans(ears_timeliness)

# ==================================================
# 4) EpiEstim on non-smoothed series
# ==================================================
epi_ns <- run_epiestim(
  series = case$Case,
  probs = c(0.00002, 0.00001, 0.0002, 0.0005, 0.005),
  dates = dates,
  background_index = background_index,
  start_idx = start_ns,
  end_idx = end_ns,
  mean_si = 3,
  std_si = 1.4,
  threshold = 1,
  pad_n = 7
)

epi_ns$fpr
epi_ns$timeliness

# ==================================================
# 5) EpiEstim on smoothed series
# ==================================================
epi_s <- run_epiestim(
  series = mah_all,
  probs = seq(0.01, 0.05, 0.01),
  dates = dates,
  background_index = background_index,
  start_idx = start_s,
  end_idx = end_s,
  mean_si = 3,
  std_si = 1.4,
  threshold = 1,
  pad_n = 7
)

epi_s$fpr
epi_s$timeliness