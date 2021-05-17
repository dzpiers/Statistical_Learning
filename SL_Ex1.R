
credit <- read.csv("credit.csv", header = TRUE, sep=",")

credit$Gender <- as.factor(credit$Gender)
credit$Education <- as.factor(credit$Education)
credit$Student <- as.factor(credit$Student)
credit$Married <- as.factor(credit$Married)
credit$Ethnicity <- as.factor(credit$Ethnicity)

## All of them
reg1 <- lm(Balance~Income+Limit+Rating+Cards+Age+Education+Gender+Student+Married+Ethnicity,data=credit)
summary(reg1)

## No Ethnicity
reg2 <- lm(Balance~Income+Limit+Rating+Cards+Age+Education+Gender+Student+Married,data=credit)
summary(reg2)


reg2 <- lm(Balance~Income+Limit+Rating+Cards+Age+Education+Gender+Student,data=credit)
summary(reg2)

reg3 <- lm(Balance~Income+Limit+Rating+Cards+Age+Education+Student,data=credit)
summary(reg3)

reg4 <- lm(Balance~Income+Limit+Rating+Cards+Age+Student,data=credit)
summary(reg4)