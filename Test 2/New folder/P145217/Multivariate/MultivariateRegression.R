ami_data <- read.table("http://static.lib.virginia.edu/statlab/materials/data/ami_data.DAT")
names(ami_data) <- c("TOT","AMI","GEN","AMT","PR","DIAP","QRS")

summary(ami_data)
pairs(ami_data)

str(ami_data)

mlm1 <- lm(cbind(TOT, AMI) ~ GEN + AMT + PR + DIAP + QRS, data = ami_data)
summary(mlm1)

m1 <- lm(TOT ~ GEN + AMT + PR + DIAP + QRS, data = ami_data)
summary(m1)
m2 <- lm(AMI ~ GEN + AMT + PR + DIAP + QRS, data = ami_data)
summary(m2)

head(resid(mlm1))

head(fitted(mlm1))

coef(mlm1)

sigma(mlm1)

vcov(mlm1)
vcov(lm(lm(TOT ~ GEN + AMT + PR + DIAP + QRS, data = ami_data)))
