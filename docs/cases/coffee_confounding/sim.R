library(stom)

N = 500
A = rnorm(N)
C_lambda = 1/logistic(A+1.5)
C = rpois(N, C_lambda)
H = rnorm(N, -A)

traj = lm(H ~ C)
plot(C,H, 
     xlab="Coffee consumption (cups/day)", 
     ylab="Health")
a = traj$coefficients[1]; b = traj$coefficients[2]
abline(a, b, col=2)
text(6.5, .7+a+b*6.5, paste("r =",cor(C,H) |> round(3)), col=2)
