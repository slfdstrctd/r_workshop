# Title     : Контрольная работа
# Created by: slfdstrctd
# Created on: 1/22/2021
library(PerformanceAnalytics)
library(samplingbook)
library(EnvStats)

# Загрузка данных
data <- read.csv(file = 'data/data.csv')
s1 <- read.csv(file = 'data/s1.csv')
s2 <- read.csv(file = 'data/s2.csv')


# Генерация случайных выборок
#randomRows <- function(df, m) {
#  return(df[sample(nrow(df), m),])
#}
#
#s1 <- randomRows(data, 100)
#s2 <- randomRows(data, 400)
#
#write.csv(s1, 's1.csv')
#write.csv(s2, 's2.csv')


# ===== 1 =====
# Размер dataset
n <- nrow(data)
par(mfrow = c(3, 2))
# РассматSриваем признак key (0..11) [тональность]
# Сгруппированный ряд
t <- table(data$key)
p <- t / n
print(t)
x <- as.integer(names(t))

# ===== 2 =====
# Эмпирическая функция распределения
F <- ecdf(data$key)
jpeg("images/1.jpg", width = 350, height = 350)
plot(F, lwd = 2)
lines(x, cumsum(p), type = "l", col = "red", lwd = 2) # кумулята
dev.off()

# ===== 3 =====
jpeg("images/2.jpg", width = 350, height = 350)
hist(data$acousticness, freq = FALSE)
dev.off()

# ===== 4 =====
# Смещённая дисперсия (/n)
var_N <- function(x) { var(x) * ((length(x) - 1) / length(x)) }

cat("\nМатожидание\ndata \t s_1 \t s_2\n",
    c(mean(data$key), mean(s1$key), mean(s2$key))) # матожидание

cat("\nДисперсия, [испр]\ndata \t\t s_1 \t\t\t\t s_2\n",
    var_N(data$key), var_N(s1$key), "[", var(s1$key), "]", var_N(s2$key), "[", var(s2$key), "]")

cat("\nСр. кв. отклонение, [испр]\ndata \t\t s_1 \t\t\t\t s_2\n",
    sqrt(c((var_N(data$key)), var_N(s1$key))), "[", sd(s1$key), "]", sqrt(var_N(s2$key)), "[", sd(s2$key), "]")

cat("\nАсимметрия\ndata \t\t s_1 \t s_2\n",
    c(skewness(data$key), skewness(s1$key), skewness(s2$key)))

cat("\nЭксцесс\ndata \t\t s_1 \t s_2\n",
    c(kurtosis(data$key), kurtosis(s1$key), kurtosis(s2$key)))

# ===== 5 =====
cat("\n\nnorm test")

norm.interval <- function(data, variance = var(data), conf.level = 0.95) {
  z <- qnorm((1 - conf.level) / 2, lower.tail = FALSE)
  xbar <- mean(data)
  sdx <- sqrt(variance / length(data))
  c(xbar - z * sdx, xbar + z * sdx) }

cat("\ns1 (10000): ", norm.interval(s1$key, var(s1$key)))
cat("\ns2 (30): ", norm.interval(s2$key, var(s2$key)))
cat("\n\nt test")
cat("\ns1 (10000): ", c(t.test(s1$key, conf.level = 0.95)$conf.int[1], t.test(s1$key, conf.level = 0.95)$conf.int[2]))
cat("\ns2 (30): ", c(t.test(s2$key, conf.level = 0.95)$conf.int[1], t.test(s2$key, conf.level = 0.95)$conf.int[2]))

# Доверительные интервалы дисперсии
cat("\n\nvar test")

var.interval <- function(data, conf.level = 0.95) {
  df <- length(data) - 1
  chilower <- qchisq((1 - conf.level) / 2, df)
  chiupper <- qchisq((1 - conf.level) / 2, df, lower.tail = FALSE)
  v <- var(data)
  c(df * v / chiupper, df * v / chilower)
}

cat("\ns1 (10000): ", var.interval(s1$key))
cat("\ns2 (30): ", var.interval(s2$key))

# ===== 6 =====

cat("\n\nsample size (0.001")
cat("\ndata: ", sample.size.mean(0.001, sqrt(var_N(data$key)))$n)
cat("\ns1: ", sample.size.mean(0.001, sd(s1$key))$n)
cat("\ns2: ", sample.size.mean(0.001, sd(s2$key))$n, "\n")
cat("\n\nsample size (0.0001")
cat("\ndata: ", sample.size.mean(0.0001, sqrt(var_N(data$key)))$n)
cat("\ns1: ", sample.size.mean(0.0001, sd(s1$key))$n)
cat("\ns2: ", sample.size.mean(0.0001, sd(s2$key))$n, "\n")

# ===== 7 =====
# Проверка на нормаьность по критерию Пирсона
t1 <- table(s1$key)
m1 <- mean(s1$key)
v1 <- var(s1$key)
sd1 <- sd(s1$key)
df <- length(s1$key) - 1
z <- (x - m1) / v1
uu <- pnorm(z)
pi <- NULL
pi[1] <- uu[1]
for (i in 1:(length(t1) - 1)) {
  pi <- append(pi, uu[i + 1] - uu[i])
}

pn <- as.vector(pi * sum(t1)) # теор частоты
dif <- pn - t1 # разница
cat("\nX-squared: ", sum(dif^2 / pn)) # вычисленное
cat("\ncrit: ", qchisq(.05, df = df, lower.tail = FALSE)) # критическое значение

jpeg("images/3.jpg", width = 350, height = 350)
hist(s1$key, freq = FALSE)
curve(dnorm(x, mean = mean(s1$key), sd = sd(s1$key)), col = "red", lwd = 2, from = 0, to = 11, add = T)
dev.off()

# Критерий Колмогорова (gof)
ks.test(s1$acousticness, punif)
cdf1 <- ecdf(s1$acousticness)
cdf2 <- ecdf(s2$acousticness)
pu <- punif(s1$acousticness)
minMax <- seq(min(s1$acousticness, s2$acousticness), max(s1$acousticness, s2$acousticness), length.out = length(s1$acousticness))
x0 <- minMax[which(abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))))]
y0 <- cdf1(x0)
y1 <- cdf2(x0)

jpeg("images/4.jpg", width = 350, height = 350)
plot(cdf1, verticals = TRUE, do.points = FALSE, col = "blue")
curve(punif(x), col = "red", add = TRUE, lwd = 2)
dev.off()

# ===== 8 =====
# Критерий Колмогорова–Смирнова для двух выборок
ks.test(s1$acousticness, s2$acousticness)
jpeg("images/5.jpg", width = 350, height = 350)
plot(cdf1, verticals = TRUE, do.points = FALSE, col = "blue")
plot(cdf2, verticals = TRUE, do.points = FALSE, col = "green", add = TRUE)
points(c(x0, x0), c(y0, y1), pch = 16, col = "red")
segments(x0, y0, x0, y1, col = "red", lty = "dotted")
dev.off()

# ===== 9 =====
# Гипотеза о матожидании
mu <- 4
alpha <- 0.01
crit1 <- qnorm(alpha)
z <- (mean(s1$key) - mu) * sqrt(length(s1$key)) / sqrt(var_N(data$key))
cat("mu0 =", mu, "\nz = ", z, "\ncrit: ", crit1)

# ===== 10 =====
# Гипотеза о дисперсии
var1 <- 3
alpha <- 0.01
df <- length(s1$key) - 1
crit2 <- qchisq(alpha, df, lower.tail = FALSE)
chi <- df * sd(s1$key) / var1
cat("\n\nvar0 = ", var1, "\nchi = ", chi, "\ncrit: ", crit2)

# ===== 11 =====
t.test(s1$key, s2$key, var.equal = TRUE)
var.test(s1$key, s2$key)


#m1 <- 100
#n1 <- 1000
#p0 <- 0.08
#alpha <- 0.05
#u <- (m1 / n1 - p0) * sqrt(n1) / sqrt(p0 * (1 - p0))
#u
##(1 - 2 * alpha) / 2
#qnorm(0.05, lower.tail = FALSE)
#pnorm(u, lower.tail = FALSE)