alpha <- 0
beta <- 0
gamma <- 0
delta <- 0

x <- 0.1
z <- 0.5

alpha <- c(3.1, 0.57, 2.83, 1.8)
beta <- c(-0.014, -0.039, -0-034, -0.032)
gamma <- c(-10.34, -10.76, -9.83, -11.62)
delta <- 33.78

x <- 1.51
z <- 0.95



area <- 100

lambda <- exp(alpha + beta * x)

bias <- exp(gamma + delta * z)

# lambda_count <- exp(log(lambda))
lambda_count <- lambda

# lambda_po    <- exp(log(lambda) + log(bias) + log(area))
lambda_po <- lambda * bias * area

# p_pa <- 1 - exp(-exp(log(lambda)))
p_pa <- 1 - exp(-lambda)

lambda_po
p_pa
lambda_count


expand_grid(
   alpha = seq(from = -3, to = 3, by = 0.5),
   beta  = seq(from = -3, to = 3, by = 0.5),
   gamma = seq(from = -3, to = 3, by = 0.5),
   delta = seq(from = -3, to = 3, by = 0.5),
   x     = seq(from = -3, to = 3, by = 0.5),
   z     = seq(from = 0, to = 1, by = 0.1)
)


# biases
plot(seq(0, 1, by = 0.01), exp(-10.34 + 33.78*seq(0, 1, by = 0.01)))

plot(seq(0, 1, by = 0.01), exp(-3.6 + 3.8*seq(0, 1, by = 0.01)))
