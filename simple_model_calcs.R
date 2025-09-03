alpha <- 0
beta <- 0

gamma <- 0
delta <- 30

x <- 0.1
z <- 0.5

area <- 100

lambda <- exp(alpha + beta * x)

bias <- exp(gamma + delta * z)

lambda_count <- lambda

lambda_po    <- exp(log(lambda) + log(bias) + log(area))

p_pa <- 1 - exp(-exp(log(lambda)))

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
