# Function to safely remove a worksheet if it exists
remove_sheet_safely <- function(wb, sheet_name) {
  existing_sheets <- names(wb)
  if (sheet_name %in% existing_sheets) {
    tryCatch({
      removeWorksheet(wb, sheet_name)
    }, error = function(e) {
      message(paste("⚠️ Could not remove sheet:", sheet_name))
    })
  }
}

# Generate Weights
generate_weights <- function(n, B, seed=NULL) {
  
  set.seed(seed)                        # optional: reproducible draws
  weights_mat <- replicate(B, {        # each column: Exp(1)
    w <- -log(runif(n))                # raw weights
    w * n / sum(w)
  })
  weights_mat
}



draw_ranking <- function(alpha) {
  
  N <- length(alpha)
  
  ## randomly pick 1, 2 or 3 (each with probability 1/3)
  first_draw <- sample(1:3, size = 1)
  
  ## randomly pick 4 distinct integers from 4 … 164 (all equally likely)
  second_draw <- sample(4:164, size = 2, replace = FALSE)
  
  draws <- c(first_draw, second_draw)
  
  # alpha_sub  – named numeric vector of worths for the current firms
  remaining <- draws
  remaining_prob <- alpha[draws]
  ranking   <- c()           # will hold the ordered firm labels
  
  while (length(remaining) > 1) {
    picked <- sample(remaining, size = 1, prob = remaining_prob)
    ranking <- c(ranking, picked)     # append winner
    remaining <- remaining[remaining != picked ]
    remaining_prob <- alpha[remaining]
  }
  ranking <- c(ranking,remaining)
  return(ranking)                     # character vector of firm labels
}


## helper that returns a pair of data frames -------------------------------
make_ranking <- function(N, K, alpha) {
  
  M <- matrix(0L, nrow = N, ncol = K)
  for (i in seq_len(N)) {
    M[i, draw_ranking(alpha)] <- 1:3
  }
  
  # repeat {                                 # <- keep trying until condition met
  #   ## 1.  fresh zero matrices
  #   
  #   
  #   ## 2.  fill one ranking per respondent
  # 
  #   
  #   ## 3.  check the “≥2 positives per column” rule
  #   ok <- all(colSums(M > 0L) >= 2L)
  #   
  #   if (ok) {                      # success ─ leave the repeat–loop
  #     break
  #   }
  #   ## else: loop again and create two new matrices
  # }
  
  ## 4.  convert to data.frames & return
  df <- as.data.frame(M)
  return(df)
}



true_theta <- function(K) {
  
  set.seed(123)
  
  ## ---------- 0.  Fixed latent worths  ---------------------
  # items per question
  mu <- 0
  Sigma <- 1
  theta_latent <- MASS::mvrnorm(K-1, mu, Sigma)          # (K-1) × 2
  theta <- c(0, theta_latent[,1])  
  # 
  # remove_sheet_safely(wb, "true_params")
  # addWorksheet(wb, "true_params")
  # writeData(wb, sheet = "true_params", x = theta)
  return(theta)
}
