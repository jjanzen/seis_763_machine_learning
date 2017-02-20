# R basics (similar to matlab for lecture 2)
# seis763-02, Spring 2017
# by josh janzen
# there is more than 1 way to do these operations (and I'm not saying this is the best way)

# assign variables
a <- 5
b <- 3

# addition
a + b

# infinity
c = Inf

# 3x4 matrix of ones
matrix(c(1),nrow=3,ncol=4)

# numeric vector
vector(mode="numeric", length=3)

# matrix 1-6 (filled by column)
matrix(1:6, nrow=2)

# matrix 1-6 (filled by row)
matrix(1:6, nrow=2, ncol=3, byrow=T)

# stacking 1s on top of matrix
s <- matrix(1:6, nrow=2, ncol=3, byrow=TRUE)
st <- rbind(c(1,1,1),s)
st

# stacking 7-9 on bottom of matrix
stu <- rbind(st,(7:9))
stu

# 2nd and 3rd rows, 1st and 2nd columns
stu[2:3,1:2]

# 3rd and 4th rows, 2nd and 3rd columns
stu[3:4,2:3]

# get last two rows and last two columns
stu[(nrow(stu)-1):nrow(stu),(ncol(stu)-1):ncol(stu)]

# element 3 down and 2 over 
stu[3,2]

# setting 3rd row as NA
stu[3,] <- NA
stu

# empty matrix
data <- data.frame()

# create 2 x 3 matrix, filled by specified vector
z <- matrix(c(1,1,1,2,3,4), nrow=2, byrow=T)

# dimensions of matrix
dim(z)

# transpose matrix
(t(z))

# multiples variable 5 by matrix
a * z

# 3 columns 10 rows from normal dist
replicate(3, rnorm(10)) 

# for loop example 1
for (year in c(2010,2011,2012,2013,2014,2015)){
  print(paste("The year is", year))
}

# for loop example 2
years <- c(2010,2011,2012,2013,2014,2015)
for (year in 1:length(years)){
  print(paste("The year is", years[year]))
}

# matrix mutiplication
theta <- matrix(c(0,4), ncol=1)
X <- matrix(c(1,1,1,1,2,3), ncol=3, nrow=2, byrow=T)
t(theta) %*% X

# matrix mutiplication slide 6 from Linear Regression
theta <- matrix(c(0,4), ncol=1)
X <- matrix(c(1,1,1,1,2,3), ncol=3, nrow=2, byrow=T)
t(theta) %*% X

# example linear regression
X <- c(1,2,3)
Y <- c(4,8,12)

reg_model = lm(Y ~ X)
reg_model
summary(reg_model)
plot(X,Y)

# example linear regression
X1 <- c(1,2,3)
Y <- c(5,8,10)
X2 <- X1^2 + c(10,10,10)

df_model <- as.data.frame(cbind(X1,X2,Y))
df_model 
class(df_model)
reg_model <- lm(Y ~ X1 + X2, data=df_model)
reg_model
summary(reg_model)
plot(X_new,Y)
plot(X,Y)
predictions <- predict(reg_model, newdata=c(4,5), interval="predict")
predictions

summary(reg_model)$coefficients[,1]

# exp linear regression polynomial
poly_model <- lm(Y ~ poly(X,2))
summary(poly_model)


## Predictions
x <- rnorm(15)
x
y <- x + rnorm(15)
y
predict(lm(y ~ x))
summary(lm(y~x))
new <- data.frame(x = seq(-3, 3, 0.5))
new
predict(lm(y ~ x), new, se.fit = TRUE)
