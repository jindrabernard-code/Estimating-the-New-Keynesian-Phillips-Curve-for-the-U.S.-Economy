# Promazani promennych a konzole

rm(list = ls())

cat("\014")

# Instalace a nacitani potrebnych balicku ==================

#   install.packages("readxl")

library(readxl)

#  install.packages("dplyr")

library(dplyr)

#  install.packages("ggplot2")

library(ggplot2)

#  install.packages("zoo")

library(zoo)

#   install.packages("car")

library(car)

# Balicek ggforify (rozsireni ggplot2 pro casove rady)

#  install.packages("ggfortify")

library(ggfortify)

# Balicek dynlm pro regresni modely s casovymi radami

#  install.packages("dynlm")

library(dynlm)

# balik gridextra (pro kombinaci grafiky z ggplot)

#  install.packages("gridExtra")

library(gridExtra)

# balik tstools

#   install.packages("tstools")

library(tstools)

# balik stargazer (pro pekny vystup tabulek)

#  install.packages("stargazer")

library(stargazer)

# balik strucchange (pro testy strukturalniho zlomu)

#   install.packages("strucchange")

library(strucchange)

# balik maxLik (pro maximalne verohodny odhad)

#  install.packages("maxLik")

library(maxLik)

# balik sandwich (pro robustni odhady)

#   install.packages("sandwich")

library(sandwich)

# MASS (moderni aplikovana statistika)

library(MASS)

# mFilter s filtracnimi technikami (mj. H-P filtr)

#  install.packages("mFilter")

library(mFilter)

# Balicek pro korelogramy apod.

#  install.packages("forecast")

library(forecast)

# knihovna gmm (pro estimator GMM)

#   install.packages("gmm")

library(gmm)

# Nacteni datoveho souboru, tvorba a vykresleni rad  ==========

# Obdobi: 1990Q1 - 2025Q4 (ctvrtletni data)

my_data <- read_excel("NKPC_data_USA_FRED.xlsx")

# prevedeni na ts objekt (casove rady), zacatek 1990 Q1

ts_data <- ts(my_data[, -1], start = c(1990, 1), frequency = 4)

# 1. ukol ================

# Vytvoreni modelovych promennych

# Inflace: anualizovana log-zmena GDP deflátoru

# 4 * diff(log(P)) prevadi ctvrtletni zmenu na rocni miru inflace

pi_def <- 4 * diff(log(ts_data[, "Deflator"]))

# Mezera vystupu: H-P filtr na logaritmus realneho HDP (lambda = 1600 pro ctvrtletni data)

# cycle = cyklicka slozka = procentni odchylka od trendu (mezera vystupu)

pom <- log(ts_data[, "Realne_HDP"])

pom_hp <- hpfilter(pom, freq = 1600, type = "lambda", drift = FALSE)

gap <- pom_hp$cycle

# Realne mezni naklady: H-P filtr na logaritmus ULC (jednotkove naklady prace)

pom <- log(ts_data[, "ULC"])

pom_hp <- hpfilter(pom, freq = 1600, type = "lambda", drift = FALSE)

ulc <- pom_hp$cycle

# Urokove rozpeti: rozdil dlouhodobe a kratkodobe sazby

spread <- (ts_data[, "IR_Long"] - ts_data[, "IR_Short"]) / 100

# Aproximace "mzdove" inflace: log-zmena ULC (hladina, ne cyklicka slozka)

pi_w <- diff(log(ts_data[, "ULC"]))

# ts objekt se vsemi transformovanymi promennymi (sjednoceni a oriznuty na spolecne obdobi)

ts_y <- ts.union(pi_def, gap, ulc, spread, pi_w)

ts_y <- window(ts_y, start = c(1990, 3), end = c(2025, 4))

# Vykresleni modelovych rad

autoplot(ts_y, colour = TRUE, facets = TRUE, size = 1) +

    ggtitle("Modelové proměnné NKPC")

# Krizovy korelogram: inflace vs. mezera vystupu

ggCcf(ts_y[, "gap"], ts_y[, "pi_def"],

      lag.max = 20,

      type = "correlation",

      color = "steelblue") +

    ggtitle("Křížový korelogram: inflace vs. mezera výstupu")

# Krizovy korelogram: inflace vs. realne mezni naklady (ULC)

ggCcf(ts_y[, "ulc"], ts_y[, "pi_def"],

      lag.max = 20,

      type = "correlation",

      color = "darkred") +

    ggtitle("Křížový korelogram: inflace vs. reálné mezní náklady (ULC)")

# 2. ukol =============

# Vytvoreni datovych matic pro GMM odhad

yy <- ts_y[, "pi_def"]

XX <- ts.union(ts_y[, "ulc"], stats::lag(ts_y[, "pi_def"], k = -1))

Z_1 <- stats::lag(ts_y[, c("pi_def", "gap", "ulc", "spread", "pi_w")], k = 1)

ZZ <- ts.union(Z_1)

k_X <- dim(XX)[2]

k_Z <- dim(ZZ)[2]

pom_data <- ts.union(yy, XX, ZZ)

pom_data <- na.omit(pom_data)

# Vpred hledici NKPC ve strukturalni forme  ====================

x_mat <- matrix(pom_data, ncol = (1 + k_X + k_Z))

my_g <- function(my_par, x) {

    theta <- my_par[1]

    beta  <- my_par[2]

    e <- theta * x[, 1] - (1 - theta) * (1 - beta * theta) * x[, 2] - theta * beta * x[, 3]

    Z <- x[, (1 + k_X + 1):(1 + k_X + k_Z)]

    gmat <- as.numeric(e) * Z

    return(gmat)

}

theta_0 <- 0.5

beta_0  <- 0.95

Model_1 <- gmm(my_g, x = x_mat, t0 = c(theta_0, beta_0), type = "twoStep")

summary(Model_1)

theta  <- coefficients(Model_1)[1]

beta   <- coefficients(Model_1)[2]

lambda <- (1 - theta) * (1 - beta * theta) / theta

writeLines(sprintf("\n Vpred hledici strukturalni: lambda = %.4f \n", lambda))

x <- x_mat[, 1:(1 + k_X)]

pom_e <- theta * x[, 1] - (1 - theta) * (1 - beta * theta) * x[, 2] - theta * beta * x[, 3]

ggAcf(pom_e, lag.max = 20, size = 1, color = "red") +

    ggtitle("ACF rezidui: vpřed hledící strukturální")

# Vpred hledici NKPC v redukovane forme ======================

my_g2 <- function(my_par, x) {

    lambda <- my_par[1]

    beta   <- my_par[2]

    e <- x[, 1] - lambda * x[, 2] - beta * x[, 3]

    Z <- x[, (1 + k_X + 1):(1 + k_X + k_Z)]

    gmat <- as.numeric(e) * Z

    return(gmat)

}

x_mat <- matrix(pom_data, ncol = (1 + k_X + k_Z))

lambda_0 <- 0.05

beta_0   <- 0.95

Model_2 <- gmm(my_g2, x = x_mat, t0 = c(lambda_0, beta_0), type = "twoStep")

summary(Model_2)

lambda <- coefficients(Model_2)[1]

beta   <- coefficients(Model_2)[2]

x <- x_mat[, 1:(1 + k_X)]

pom_e <- x[, 1] - lambda * x[, 2] - beta * x[, 3]

ggAcf(pom_e, lag.max = 20, size = 1, color = "blue") +

    ggtitle("ACF rezidui: vpřed hledící redukovaná")

# Hybridni NKPC v redukovane forme ==============

XX <- ts.union(ts_y[, "ulc"],

               stats::lag(ts_y[, "pi_def"], k = -1),

               stats::lag(ts_y[, "pi_def"], k = 1))

k_X <- dim(XX)[2]

pom_data <- ts.union(yy, XX, ZZ)

pom_data <- na.omit(pom_data)

x_mat <- matrix(pom_data, ncol = (1 + k_X + k_Z))

my_g3 <- function(my_par, x) {

    lambda  <- my_par[1]

    gamma_f <- my_par[2]

    gamma_b <- my_par[3]

    e <- x[, 1] - lambda * x[, 2] - gamma_f * x[, 3] - gamma_b * x[, 4]

    Z <- x[, (1 + k_X + 1):(1 + k_X + k_Z)]

    gmat <- as.numeric(e) * Z

    return(gmat)

}

lambda_0  <- 0.05

gamma_f_0 <- 0.6

gamma_b_0 <- 0.3

Model_3 <- gmm(my_g3, x = x_mat, t0 = c(lambda_0, gamma_f_0, gamma_b_0), type = "twoStep")

summary(Model_3)

lambda  <- coefficients(Model_3)[1]

gamma_f <- coefficients(Model_3)[2]

gamma_b <- coefficients(Model_3)[3]

x <- x_mat[, 1:(1 + k_X)]

pom_e <- x[, 1] - lambda * x[, 2] - gamma_f * x[, 3] - gamma_b * x[, 4]

ggAcf(pom_e, lag.max = 20, size = 1, color = "#9F16E9") +

    ggtitle("ACF rezidui: hybridní redukovaná")

# Hybridni NKPC ve strukturalni forme ================

my_g4 <- function(my_par, x) {

    theta <- my_par[1]

    beta  <- my_par[2]

    omega <- my_par[3]

    phi     <- theta + omega * (1 - theta * (1 - beta))

    lambda  <- (1 - omega) * (1 - theta) * (1 - beta * theta) * phi^-1

    gamma_f <- beta * theta * phi^-1

    gamma_b <- omega * phi^-1

    e <- x[, 1] - lambda * x[, 2] - gamma_f * x[, 3] - gamma_b * x[, 4]

    Z <- x[, (1 + k_X + 1):(1 + k_X + k_Z)]

    gmat <- as.numeric(e) * Z

    return(gmat)

}

omega_0 <- 0.5

Model_4 <- gmm(my_g4, x = x_mat, t0 = c(theta_0, beta_0, omega_0), type = "twoStep")

summary(Model_4)

theta <- coefficients(Model_4)[1]

beta  <- coefficients(Model_4)[2]

omega <- coefficients(Model_4)[3]

phi     <- theta + omega * (1 - theta * (1 - beta))

lambda  <- (1 - omega) * (1 - theta) * (1 - beta * theta) * phi^-1

gamma_f <- beta * theta * phi^-1

gamma_b <- omega * phi^-1

writeLines(sprintf("\n Hybridni strukturalni: lambda=%.4f, gamma_f=%.4f, gamma_b=%.4f \n",

                   lambda, gamma_f, gamma_b))

x <- x_mat[, 1:(1 + k_X)]

pom_e <- x[, 1] - lambda * x[, 2] - gamma_f * x[, 3] - gamma_b * x[, 4]

ggAcf(pom_e, lag.max = 20, size = 1, color = "#0D401B") +

    ggtitle("ACF rezidui: hybridní strukturální")

# 3. ukol =========

# Omezene modely s beta = 1 (test dlouhodobe vertikality NKPC)

XX <- ts.union(ts_y[, "ulc"], stats::lag(ts_y[, "pi_def"], k = -1))

k_X <- dim(XX)[2]

pom_data <- ts.union(yy, XX, ZZ)

pom_data <- na.omit(pom_data)

x_mat <- matrix(pom_data, ncol = (1 + k_X + k_Z))

my_g_r1 <- function(my_par, x) {

    theta <- my_par[1]

    beta  <- 1

    e <- theta * x[, 1] - (1 - theta) * (1 - beta * theta) * x[, 2] - theta * beta * x[, 3]

    Z <- x[, (1 + k_X + 1):(1 + k_X + k_Z)]

    gmat <- as.numeric(e) * Z

    return(gmat)

}

theta_0 <- 0.5

Model_1r <- gmm(my_g_r1, x = x_mat, t0 = theta_0, type = "twoStep")

summary(Model_1r)

theta  <- coefficients(Model_1r)[1]

beta   <- 1

lambda <- (1 - theta) * (1 - beta * theta) / theta

writeLines(sprintf("\n Vpred hledici strukturalni (beta=1): lambda = %.4f \n", lambda))

my_g2_r <- function(my_par, x) {

    lambda <- my_par[1]

    beta   <- 1

    e <- x[, 1] - lambda * x[, 2] - beta * x[, 3]

    Z <- x[, (1 + k_X + 1):(1 + k_X + k_Z)]

    gmat <- as.numeric(e) * Z

    return(gmat)

}

x_mat    <- matrix(pom_data, ncol = (1 + k_X + k_Z))

lambda_0 <- 0.05

Model_2r <- gmm(my_g2_r, x = x_mat, t0 = lambda_0, type = "twoStep")

summary(Model_2r)

XX <- ts.union(ts_y[, "ulc"],

               stats::lag(ts_y[, "pi_def"], k = -1),

               stats::lag(ts_y[, "pi_def"], k = 1))

k_X <- dim(XX)[2]

pom_data <- ts.union(yy, XX, ZZ)

pom_data <- na.omit(pom_data)

x_mat <- matrix(pom_data, ncol = (1 + k_X + k_Z))

my_g3_r <- function(my_par, x) {

    lambda  <- my_par[1]

    gamma_f <- 1

    gamma_b <- my_par[2]

    e <- x[, 1] - lambda * x[, 2] - gamma_f * x[, 3] - gamma_b * x[, 4]

    Z <- x[, (1 + k_X + 1):(1 + k_X + k_Z)]

    gmat <- as.numeric(e) * Z

    return(gmat)

}

lambda_0  <- 0.05

gamma_b_0 <- 0.3

Model_3r <- gmm(my_g3_r, x = x_mat, t0 = c(lambda_0, gamma_b_0), type = "twoStep")

summary(Model_3r)

lambda  <- coefficients(Model_3r)[1]

gamma_f <- 1

gamma_b <- coefficients(Model_3r)[2]

x <- x_mat[, 1:(1 + k_X)]

pom_e <- x[, 1] - lambda * x[, 2] - gamma_f * x[, 3] - gamma_b * x[, 4]

ggAcf(pom_e, lag.max = 20, size = 1, color = "#9F16E9") +

    ggtitle("ACF rezidui: hybridní redukovaná (beta=1)")

my_g4_r <- function(my_par, x) {

    theta <- my_par[1]

    beta  <- 1

    omega <- my_par[2]

    phi     <- theta + omega * (1 - theta * (1 - beta))

    lambda  <- (1 - omega) * (1 - theta) * (1 - beta * theta) * phi^-1

    gamma_f <- beta * theta * phi^-1

    gamma_b <- omega * phi^-1

    e <- x[, 1] - lambda * x[, 2] - gamma_f * x[, 3] - gamma_b * x[, 4]

    Z <- x[, (1 + k_X + 1):(1 + k_X + k_Z)]

    gmat <- as.numeric(e) * Z

    return(gmat)

}

omega_0 <- 0.5

Model_4r <- gmm(my_g4_r, x = x_mat, t0 = c(theta_0, omega_0), type = "twoStep")

summary(Model_4r)

theta <- coefficients(Model_4r)[1]

beta  <- 1

omega <- coefficients(Model_4r)[2]

phi     <- theta + omega * (1 - theta * (1 - beta))

lambda  <- (1 - omega) * (1 - theta) * (1 - beta * theta) * phi^-1

gamma_f <- beta * theta * phi^-1

gamma_b <- omega * phi^-1

writeLines(sprintf("\n Hybridni strukturalni (beta=1): lambda=%.4f, gamma_f=%.4f, gamma_b=%.4f \n",

                   lambda, gamma_f, gamma_b))

x <- x_mat[, 1:(1 + k_X)]

pom_e <- x[, 1] - lambda * x[, 2] - gamma_f * x[, 3] - gamma_b * x[, 4]

ggAcf(pom_e, lag.max = 20, size = 1, color = "#0D401B") +

    ggtitle("ACF rezidui: hybridní strukturální (beta=1)")

# 4. ukol #################

# Odhad metodou NLS

pi_f <- stats::lag(pi_def, k = -1)

pi_b <- stats::lag(pi_def, k = 1)

mc   <- ulc

data_nls <- na.omit(cbind(pi_def, mc, pi_f, pi_b))

colnames(data_nls) <- c("pi_def", "mc", "pi_f", "pi_b")

df_nls <- as.data.frame(data_nls)

nls_model <- nls(pi_def ~ lambda * mc + gamma_f * pi_f + gamma_b * pi_b,

                 data = df_nls,

                 start = list(lambda = 0.05, gamma_f = 0.6, gamma_b = 0.3))

summary(nls_model)

SSR_U    <- sum(resid(nls_model)^2)

loglik_U <- -0.5 * length(resid(nls_model)) * log(SSR_U)

nls_model_r <- nls(pi_def ~ lambda * mc + gamma_f * pi_f + (1 - gamma_f) * pi_b,

                   data = df_nls,

                   start = list(lambda = 0.05, gamma_f = 0.6))

SSR_R    <- sum(resid(nls_model_r)^2)

loglik_R <- -0.5 * length(resid(nls_model_r)) * log(SSR_R)

LR_stat <- -2 * (loglik_R - loglik_U)

p_value <- pchisq(LR_stat, df = 1, lower.tail = FALSE)

cat("NLS redukovana - LR statistika =", round(LR_stat, 4),

    ", p-hodnota =", round(p_value, 4), "\n")

beta <- 0.99

theta_start <- coefficients(Model_4)[1]

omega_start <- coefficients(Model_4)[3]

nls_model_2 <- nls(

    formula = pi_def ~

        ((1 - omega) * (1 - theta) * (1 - beta * theta) /

             (theta + omega * (1 - theta * (1 - beta)))) * mc +

        ((beta * theta) / (theta + omega * (1 - theta * (1 - beta)))) * pi_f +

        ((omega) / (theta + omega * (1 - theta * (1 - beta)))) * pi_b,

    data      = df_nls,

    start     = list(theta = theta_start, omega = omega_start),

    algorithm = "port",

    control   = nls.control(maxiter = 500, tol = 1e-5)

)

summary(nls_model_2)

theta <- coefficients(nls_model_2)[1]

omega <- coefficients(nls_model_2)[2]

phi     <- theta + omega * (1 - theta * (1 - beta))

lambda  <- (1 - omega) * (1 - theta) * (1 - beta * theta) * phi^-1

gamma_f <- beta * theta * phi^-1

gamma_b <- omega * phi^-1

writeLines(sprintf("\n NLS strukturalni: lambda=%.4f, gamma_f=%.4f, gamma_b=%.4f \n",

                   lambda, gamma_f, gamma_b))

SSR_U    <- sum(resid(nls_model_2)^2)

loglik_U <- -0.5 * length(resid(nls_model_2)) * log(SSR_U)

SSR_R    <- sum(resid(nls_model_r)^2)

loglik_R <- -0.5 * length(resid(nls_model_r)) * log(SSR_R)

LR_stat <- -2 * (loglik_R - loglik_U)

p_value <- pchisq(LR_stat, df = 1, lower.tail = FALSE)

cat("NLS strukturalni - LR statistika =", round(LR_stat, 4),

    ", p-hodnota =", round(p_value, 4), "\n")

# Odhad metodou ML - normalni rozdeleni ================

data_ml <- na.omit(cbind(pi_def, mc, pi_f, pi_b))

data_ml <- as.data.frame(data_ml)

colnames(data_ml) <- c("pi_def", "mc", "pi_f", "pi_b")

lrm <- lm(pi_def ~ mc + pi_f + pi_b, data = data_ml)

pom_b   <- coefficients(lrm)

pom_sig <- summary(lrm)$sigma

loglik_norm <- function(par, data = data_ml) {

    lambda  <- par[1]

    gamma_f <- par[2]

    gamma_b <- par[3]

    sigma   <- abs(par[4])

    pi_hat  <- lambda * data[, "mc"] + gamma_f * data[, "pi_f"] + gamma_b * data[, "pi_b"]

    e       <- data[, "pi_def"] - pi_hat

    ll      <- -0.5 * log(2 * pi) - log(sigma) - (e^2) / (2 * sigma^2)

}

ml_model <- maxLik(loglik_norm,

                   start  = c(lambda = pom_b[2], gamma_f = pom_b[3],

                              gamma_b = pom_b[4], sigma = pom_sig),

                   method = "BHHH")

summary(ml_model)

loglik_U <- as.numeric(logLik(ml_model))

loglik_norm_r <- function(par, data = data_ml) {

    lambda  <- par[1]

    gamma_f <- par[2]

    sigma   <- abs(par[3])

    pi_hat  <- lambda * data[, "mc"] + gamma_f * data[, "pi_f"] + (1 - gamma_f) * data[, "pi_b"]

    e       <- data[, "pi_def"] - pi_hat

    ll      <- -0.5 * log(2 * pi) - log(sigma) - (e^2) / (2 * sigma^2)

}

lrm_r   <- lm(pi_def ~ mc + pi_f, data = data_ml)

pom_b   <- coefficients(lrm_r)

pom_sig <- summary(lrm_r)$sigma

ml_r <- maxLik(logLik = loglik_norm_r,

               start  = c(lambda = pom_b[2], gamma_f = pom_b[3], sigma = pom_sig),

               method = "BHHH")

loglik_R <- as.numeric(logLik(ml_r))

LR_stat  <- 2 * (loglik_U - loglik_R)

p_value  <- pchisq(LR_stat, df = 1, lower.tail = FALSE)

cat("ML normalni redukovana - LR statistika =", round(LR_stat, 4),

    ", p-hodnota =", round(p_value, 4), "\n")

loglik_norm_2 <- function(par, data = data_ml) {

    theta   <- par[1]

    omega   <- par[2]

    sigma   <- abs(par[3])

    beta    <- 0.99

    phi     <- theta + omega * (1 - theta * (1 - beta))

    lambda  <- (1 - omega) * (1 - theta) * (1 - beta * theta) / phi

    gamma_f <- beta * theta / phi

    gamma_b <- omega / phi

    pi_hat  <- lambda * data[, "mc"] + gamma_f * data[, "pi_f"] + gamma_b * data[, "pi_b"]

    e       <- data[, "pi_def"] - pi_hat

    ll      <- -0.5 * log(2 * pi) - log(sigma) - (e^2) / (2 * sigma^2)

}

ml_model_2 <- maxLik(logLik = loglik_norm_2,

                     start  = c(theta = 0.7, omega = 0.5, sigma = pom_sig),

                     method = "BHHH")

summary(ml_model_2)

theta <- coefficients(ml_model_2)[1]

omega <- coefficients(ml_model_2)[2]

beta  <- 0.99

phi     <- theta + omega * (1 - theta * (1 - beta))

lambda  <- (1 - omega) * (1 - theta) * (1 - beta * theta) * phi^-1

gamma_f <- beta * theta * phi^-1

gamma_b <- omega * phi^-1

writeLines(sprintf("\n ML normalni strukturalni: lambda=%.4f, gamma_f=%.4f, gamma_b=%.4f \n",

                   lambda, gamma_f, gamma_b))

loglik_U <- as.numeric(logLik(ml_model_2))

loglik_norm_2r <- function(par, data = data_ml) {

    theta   <- par[1]

    sigma   <- abs(par[2])

    beta    <- 0.99

    omega   <- (theta * (1 - beta)) / (1 - theta + theta * beta)

    phi     <- theta + omega * (1 - theta * (1 - beta))

    lambda  <- 0

    gamma_f <- beta * theta / phi

    gamma_b <- omega / phi

    pi_hat  <- lambda * data[, "mc"] + gamma_f * data[, "pi_f"] + gamma_b * data[, "pi_b"]

    e       <- data[, "pi_def"] - pi_hat

    ll      <- -0.5 * log(2 * pi) - log(sigma) - (e^2) / (2 * sigma^2)

}

ml_model_2r <- maxLik(logLik = loglik_norm_2r,

                      start  = c(theta = 0.7, sigma = pom_sig),

                      method = "BHHH")

loglik_R <- as.numeric(logLik(ml_model_2r))

LR_stat  <- 2 * (loglik_U - loglik_R)

p_value  <- pchisq(LR_stat, df = 1, lower.tail = FALSE)

cat("ML normalni strukturalni - LR statistika =", round(LR_stat, 4),

    ", p-hodnota =", round(p_value, 4), "\n")

# Odhad metodou ML - t-rozdeleni ================

loglik_t <- function(par, data = data_ml) {

    lambda  <- par[1]

    gamma_f <- par[2]

    gamma_b <- par[3]

    sigma   <- abs(par[4])

    nu      <- 5

    pi_hat  <- lambda * data[, "mc"] + gamma_f * data[, "pi_f"] + gamma_b * data[, "pi_b"]

    e       <- data[, "pi_def"] - pi_hat

    ll      <- lgamma((nu + 1) / 2) - lgamma(nu / 2) -

                0.5 * log(nu * pi * sigma^2) -

                ((nu + 1) / 2) * log(1 + (e^2) / (nu * sigma^2))

}

ml_model_3 <- maxLik(loglik_t,

                     start  = c(lambda = 0.1, gamma_f = 0.5,

                                gamma_b = 0.3, sigma = pom_sig),

                     method = "BHHH")

summary(ml_model_3)

loglik_U <- as.numeric(logLik(ml_model_3))

loglik_t_r <- function(par, data = data_ml) {

    lambda  <- par[1]

    gamma_f <- par[2]

    gamma_b <- 1 - gamma_f

    sigma   <- abs(par[3])

    nu      <- 5

    pi_hat  <- lambda * data[, "mc"] + gamma_f * data[, "pi_f"] + gamma_b * data[, "pi_b"]

    e       <- data[, "pi_def"] - pi_hat

    ll      <- lgamma((nu + 1) / 2) - lgamma(nu / 2) -

                0.5 * log(nu * pi * sigma^2) -

                ((nu + 1) / 2) * log(1 + (e^2) / (nu * sigma^2))

}

ml_model_3r <- maxLik(logLik = loglik_t_r,

                      start  = c(lambda = 0.1, gamma_f = 0.5, sigma = pom_sig),

                      method = "BHHH")

loglik_R <- as.numeric(logLik(ml_model_3r))

LR_stat  <- 2 * (loglik_U - loglik_R)

p_value  <- pchisq(LR_stat, df = 1, lower.tail = FALSE)

cat("ML t-rozdeleni redukovana - LR statistika =", round(LR_stat, 4),

    ", p-hodnota =", round(p_value, 4), "\n")

loglik_t_2 <- function(par, data = data_ml) {

    theta   <- par[1]

    omega   <- par[2]

    sigma   <- abs(par[3])

    beta    <- 0.99

    nu      <- 5

    phi     <- theta + omega * (1 - theta * (1 - beta))

    lambda  <- (1 - omega) * (1 - theta) * (1 - beta * theta) / phi

    gamma_f <- beta * theta / phi

    gamma_b <- omega / phi

    pi_hat  <- lambda * data[, "mc"] + gamma_f * data[, "pi_f"] + gamma_b * data[, "pi_b"]

    e       <- data[, "pi_def"] - pi_hat

    ll      <- lgamma((nu + 1) / 2) - lgamma(nu / 2) -

                0.5 * log(nu * pi * sigma^2) -

                ((nu + 1) / 2) * log(1 + (e^2) / (nu * sigma^2))

}

ml_model_4 <- maxLik(logLik = loglik_t_2,

                     start  = c(theta = 0.7, omega = 0.5, sigma = pom_sig),

                     method = "BHHH")

summary(ml_model_4)

theta <- coefficients(ml_model_4)[1]

omega <- coefficients(ml_model_4)[2]

phi     <- theta + omega * (1 - theta * (1 - beta))

lambda  <- (1 - omega) * (1 - theta) * (1 - beta * theta) * phi^-1

gamma_f <- beta * theta * phi^-1

gamma_b <- omega * phi^-1

writeLines(sprintf("\n ML t-rozdeleni strukturalni: lambda=%.4f, gamma_f=%.4f, gamma_b=%.4f \n",

                   lambda, gamma_f, gamma_b))

loglik_U <- as.numeric(logLik(ml_model_4))

loglik_t_2r <- function(par, data = data_ml) {

    theta   <- 1

    omega   <- par[1]

    sigma   <- abs(par[2])

    beta    <- 0.99

    nu      <- 5

    phi     <- theta + omega * (1 - theta * (1 - beta))

    lambda  <- (1 - omega) * (1 - theta) * (1 - beta * theta) / phi

    gamma_f <- beta * theta / phi

    gamma_b <- omega / phi

    pi_hat  <- lambda * data[, "mc"] + gamma_f * data[, "pi_f"] + gamma_b * data[, "pi_b"]

    e       <- data[, "pi_def"] - pi_hat

    ll      <- lgamma((nu + 1) / 2) - lgamma(nu / 2) -

                0.5 * log(nu * pi * sigma^2) -

                ((nu + 1) / 2) * log(1 + (e^2) / (nu * sigma^2))

}

ml_model_4r <- maxLik(logLik = loglik_t_2r,

                      start  = c(omega = 0.5, sigma = pom_sig),

                      method = "BHHH")

loglik_R <- as.numeric(logLik(ml_model_4r))

LR_stat  <- 2 * (loglik_U - loglik_R)

p_value  <- pchisq(LR_stat, df = 1, lower.tail = FALSE)

cat("ML t-rozdeleni strukturalni - LR statistika =", round(LR_stat, 4),

    ", p-hodnota =", round(p_value, 4), "\n")
