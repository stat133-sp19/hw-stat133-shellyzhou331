#' @title bin_choose
#' @description Computes the binomial coefficient
#' @param n number of trials
#' @param k number of successes, can be a vector
#' @return the binomial coefficient for each success
#' @examples
#' # 10 success in 20 trials
#' fac <- bin_choose(20, 10)
#'

bin_choose <- function(n, k){
  check_trials(n)
  check_success(k, n)
  fac <- factorial(n)/factorial(k)/factorial(n-k)
  return(fac)
}

#' @title bin_proability
#' @description Computes the binomial probability
#' @param trials number of trials
#' @param success number of successes, can be a vector
#' @param prob probability of success
#' @return binomial probability for each success
#' @examples
#' # 10 success in 20 trials with probability of success 0.5
#' probability <- bin_probability(10, 20, 0.5)
#'
bin_probability <- function(success, trials, prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  fac <- factorial(trials)/factorial(success)/factorial(trials - success)
  probability <- fac*(prob^success)*(1-prob)^(trials - success)
  return(probability)
}

#' @title bin_distribution
#' @description Gives a table of binomial distribution with probability
#' @param trials number of trials
#' @param prob probability of success
#' @return An object of class \code{"bindis"} or \code{"data.frame"}
#' @export
#' @examples
#' # 20 trials with probability of success 0.5
#' distribution <- bin_distribution(20, 0.5)
#'
bin_distribution <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  success <- c(0:trials)
  probability <- bin_probability(success, trials, prob)
  data <- cbind(success, probability)
  data <- as.data.frame(data)
  class(data) <- c("bindis", "data.frame")
  return(data)
}

#' @export
plot.bindis <- function(bindis){
 library(ggplot2)
 ggplot(data = bindis) + geom_col(aes(x = factor(success), y = probability)) + labs(x = "success", title = "Binomial Distribution")
}

#' @title bin_cumulative
#' @description Gives a table with cumuative density of the binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return an object of class \code{"bincum"} or \code{"data.frame"}
#' @export
#' @examples
#' # 20 trials with probability of success 0.5
#' cumulative <- bin_cumulative(20, 0.5)
#'
bin_cumulative <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  frame <- bin_distribution(trials, prob)
  frame$cumulative <- cumsum(frame$probability)
  class(frame) <- c("bincum", "data.frame")
  return(frame)
}

#' @export
plot.bincum <- function(bincum){
  plot(x = bincum$success, y = bincum$cumulative, type = "l", main = "Cumulative Distribution", xlab = "success", ylab = "Cumulative")
  points(x = bincum$success, y = bincum$cumulative)
}

#' @title bin_variable
#' @description prints the binomial variable with the parameters
#' @param trials number of trials
#' @param prob probability of success
#' @return an object of class \code{"binvar"}
#' @export
#' @examples
#' # 10 trials with probability of success 0.5
#' fac <- bin_choose(20, 10)
#'
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  binvar <- list(trials = trials, prob = prob)
  class(binvar) <- "binvar"
  return(binvar)
}

#' @export
print.binvar <- function(binvar){
  cat('"Binomial variable"\n\n')
  cat('Parameters\n')
  cat(paste("- Number of Trials:", binvar$trials), '\n')
  cat(paste("- Probability of Success:", binvar$prob), '\n')
}

#' @export
summary.binvar <- function(binvar){
  mean <- aux_mean(binvar$trials, binvar$prob)
  mode <- aux_mode(binvar$trials, binvar$prob)
  kur <- aux_kurtosis(binvar$trials, binvar$prob)
  skew <- aux_skewness(binvar$trials, binvar$prob)
  cat('"Summary Binomial"\n\n')
  cat('Parameters\n')
  cat(paste("- Number of Trials:", binvar$trials), '\n')
  cat(paste("- Probability of Success:", binvar$prob), '\n\n')
  cat('Measures\n')
  cat(paste("- Mean:", mean), '\n')
  cat(paste("- Variance:", aux_variance(binvar$trials, binvar$prob)), '\n')
  cat(paste("- Mode:", mode), '\n')
  cat(paste("- Skewness:", sprintf("%.7f", skew)), '\n')
  cat(paste("- Kurtosis:", sprintf("%.7f", kur)), '\n')
}

#' @export
print.summary.binvar <- function(binvar){
  mean <- aux_mean(binvar$trials, binvar$prob)
  mode <- aux_mode(binvar$trials, binvar$prob)
  kur <- aux_kurtosis(binvar$trials, binvar$prob)
  skew <- aux_skewness(binvar$trials, binvar$prob)
  cat('"Summary Binomial"\n\n')
  cat('Parameters\n')
  cat(paste("- Number of Trials:", binvar$trials), '\n')
  cat(paste("- Probability of Success:", binvar$prob), '\n\n')
  cat('Measures\n')
  cat(paste("- Mean:", mean), '\n')
  cat(paste("- Variance:", aux_variance(binvar$trials, binvar$prob)), '\n')
  cat(paste("- Mode:", mode), '\n')
  cat(paste("- Skewness:", sprintf("%.7f", skew)), '\n')
  cat(paste("- Kurtosis:", sprintf("%.7f", kur)), '\n')
}


#' @title bin_mean
#' @description Computes mean of the binomal distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return a numeric number represents the mean
#' @examples
#' # 20 trials with probability of success 0.5
#' mean <- bin_mean(20, 0.5)
#'
bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  number <- aux_mean(trials, prob)
  return(number)
}

#' @title bin_variance
#' @description Computes variance of the binomal distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return a numeric number represents the variance
#' @examples
#' # 20 trials with probability of success 0.5
#' variance <- bin_variance(20, 0.5)
#'
bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  number <- aux_variance(trials, prob)
  return(number)
}

#' @title bin_mode
#' @description Computes mode of the binomal distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return a numeric number represents the mode
#' @examples
#' # 20 trials with probability of success 0.5
#' mode <- bin_mode(20, 0.5)
#'
bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  number <- aux_mode(trials, prob)
  return(number)
}

#' @title bin_skewness
#' @description Computes skewness of the binomal distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return a numeric number represents the skewness
#' @examples
#' # 20 trials with probability of success 0.5
#' skewness <- bin_skewness(20, 0.5)
#'
bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  number <- aux_skewness(trials, prob)
  return(number)
}

#' @title bin_kurtosis
#' @description Computes kurtosis of the binomal distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return a numeric number represents the kurtosis
#' @examples
#' # 20 trials with probability of success 0.5
#' kurtosis <- bin_kurtosis(20, 0.5)
#'
bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  number <- aux_kurtosis(trials, prob)
  return(number)
}

# private functions for check

# check if probability is valid
check_prob <- function(prob){
  if (prob <= 1 & prob >= 0){
    TRUE
  }
  else {
    stop("Probability has to be between 0 and 1")
  }
}

# check if number of trials is valid
check_trials <- function(trials){
  if (trials %% 1 != 0 | trials < 0){
    stop("Trials has to be a non-negative integer")
  }
  else {
    TRUE
  }
}

# check if number of success is valid
check_success <- function(success, trials){
  for (i in 1:length(success)){
  if (success[i] > trials){
    stop("Invalid success")
  }
  }
TRUE

}

# private auxiliary functions

#auxilary function for mean
aux_mean <- function(trials, prob){
  mean <- trials*prob
  return(mean)
}

#auxilary function for variance
aux_variance <- function(trials, prob){
  var <- trials*prob*(1-prob)
  return(var)
}

#auxilary function for mode
aux_mode <- function(trials, prob){
  mode <- floor(trials*prob + prob)
  return(mode)
}

#auxilary function for skewness
aux_skewness <- function(trials, prob){
  skew <- (1-2*prob)/sqrt(trials*prob*(1-prob))
  return(skew)
}

#auxilary function for kurtosis
aux_kurtosis <- function(trials, prob){
  kur <- (1-6*prob*(1-prob))/(trials*prob*(1-prob))
  return(kur)
}
