#############
# HISTOGRAM #
#############
data(airquality)
hist(airquality$Temp)
# granice histograma
broj_kat <- ceiling(log(length(airquality$Temp),2))+1
d <- (max(airquality$Temp)-min(airquality$Temp))/broj_kat
granice <- min(airquality$Temp)-1/2+(0:broj_kat)*(d+1)
hist(airquality$Temp, breaks=granice, prob=TRUE)
attach(airquality)
hist(Temp, col = "darkmagenta", freq = FALSE)
# Koliko je procentualno temperatura bila manja od 70? Veca od 90?
H <- hist(Temp, prob=TRUE, plot=FALSE)
cumsum(H$density*diff(H$breaks))
# 21.57% ; 9.15%
hist(Temp, col = "darkmagenta", freq = FALSE, breaks = seq(50,110,5))
lines(density(Temp))
hist(Ozone, col = "magenta", freq = FALSE)
hist(Ozone, col = "magenta", freq = FALSE, breaks = seq(0,180,5))
lines(density(na.omit(Ozone)))
hist(Solar.R, col = "pink3")
hist(Solar.R, col = "pink3", freq = FALSE, breaks = seq(0,360,50))
lines(density(na.omit(Solar.R)))
hist(Wind, col = "lightblue")
hist(Wind, col = "lightblue", freq = FALSE, breaks = seq(0,22,2))
lines(density(Wind))
detach(airquality)
attach(faithful)
hist(eruptions, breaks = seq(1.4, 5.2, 0.2), prob = T, col = "orange")
lines(density(eruptions))
detach(faithful)

####################
# TABELARNI PRIKAZ #
####################
mtcars
?mtcars
summary(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)
summary(mtcars)
table(mtcars$carb)
table(mtcars$am)
table(mtcars$am, mtcars$cyl)
table(mtcars$carb, mtcars$cyl)

#####################
# TRAKASTI DIJAGRAM #
#####################
barplot(table(mtcars$carb), col = "deepskyblue", main = "Broj karburatora")
barplot(table(mtcars$carb, mtcars$cyl), col = rainbow(6), main = "Broj karburatora u zavisnosti od broja cilindara", xlab = "broj cilindara")
barplot(t(table(mtcars$carb, mtcars$cyl)), col = c("deeppink", "darkviolet","limegreen"), main = "Broj cilindara u zavisnosti od broja karburatora", xlab = "broj karburatora")
barplot(t(table(mtcars$carb, mtcars$cyl)), beside = TRUE, main = "Broj cilindara u zavisnosti od broja karburatora", xlab = "broj karburatora")

###################
# KRUZNI DIJAGRAM #
###################
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry", "Apple", "Boston Cream", "Other", "Vanilla")
pie(pie.sales, main = "Obicna pitica")
pie(pie.sales, col = gray(seq(0.4, 0.9, length = 6)), clockwise = TRUE, main = "Nijanse sive")
pie(pie.sales, col = rainbow(6), clockwise = TRUE, main = "Boje duge")

###########################
# Deskriptivne statistike #
###########################
### MERE POLOZAJA
# 1. srednja vrednost - mean()
# 2. medijana - meadian()
# 3. moda 
### MERE RASEJANJA
# 1. raspon populacije - diff(range())
# 2. disperzija (varijansa) - var()
# 3. standardno odstupanje - sd()
# 4. medjukvantilno rastojanje - quantile()

attach(mtcars)
hist(mpg, main = "Potrosnja goriva", col = "aquamarine")
hist(mpg, prob = TRUE, main = "Potrosnja goriva", col = "aquamarine")
lines(density(mpg), col = "red", lwd = 2)
mean(mpg)
median(mpg)
diff(range(mpg))
var(mpg)
sd(mpg)
quantile(mpg)
plot(mpg)
abline(mean(mpg), 0, col = "pink", lwd = 3)
###
hist(hp, main = "Konjska snaga", col = "cyan")
hist(hp, prob = TRUE, main = "Konjska snaga", col = "cyan", breaks = seq(45,350,10))
hist(hp, prob = TRUE, main = "Konjska snaga", col = "cyan", breaks = seq(45,350,20))
hist(hp, prob = TRUE, main = "Konjska snaga", col = "cyan")
lines(density(hp), col = "red", lwd = 2)
mean(hp)
median(hp)
diff(range(hp))
var(hp)
sd(hp)
quantile(hp)
plot(hp)
abline(mean(hp), 0, col = "pink", lwd = 3)
detach(mtcars)

###########
# BOXPLOT #
###########
boxplot(mtcars$mpg, main = "Potrosnja goriva", col = "aquamarine")
points(mean(mtcars$mpg), col = 'red', pch = 16)
boxplot(mtcars$hp, main = "Konjska snaga", col = "cyan")
points(mean(mtcars$hp), col = 'red', pch = 16)
boxplot(mtcars$mpg ~ mtcars$vs, main = "Potrosnja goriva u zavisnosti od vrste motora" , col ="lightblue2")
boxplot(mtcars$mpg ~ mtcars$am, main = "Potrosnja goriva u zavisnosti od vrste menjaca" , col = "lightblue")

