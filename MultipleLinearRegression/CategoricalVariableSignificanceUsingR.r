churndata <- read.csv("churn.csv")

churndata$State <- factor(churndata$State)
churndata$Churn <- factor(churndata$Churn)
churndata$Marital <- factor(churndata$Marital)
churndata$Gender <- factor(churndata$Gender)
churndata$Techie <- factor(churndata$Techie)
churndata$Contract <- factor(churndata$Contract)
churndata$Port_modem <- factor(churndata$Port_modem)
churndata$Tablet <- factor(churndata$Tablet)
churndata$InternetService <- factor(churndata$InternetService)
churndata$Phone <- factor(churndata$Phone)
churndata$Multiple <- factor(churndata$Multiple)
churndata$StreamingMovies <- factor(churndata$StreamingMovies)
churndata$PaperlessBilling <- factor(churndata$PaperlessBilling)
churndata$PaymentMethod <- factor(churndata$PaymentMethod)

mod0 <- lm(MonthlyCharge ~ Tenure+Age+Children+Bandwidth_GB_Year+Churn+State+Marital+Gender+Techie+Contract+Area+Multiple+InternetService+StreamingTV+StreamingMovies+PaymentMethod+TechSupport+OnlineSecurity+OnlineBackup+DeviceProtection, data = churndata)
summary(mod0)
# Remove State
mod1 <- lm(MonthlyCharge ~ Tenure+Age+Children+Bandwidth_GB_Year+Churn+Marital+Gender+Techie+Contract+Area+Multiple+InternetService+StreamingTV+StreamingMovies+PaymentMethod+TechSupport+OnlineSecurity+OnlineBackup+DeviceProtection, data = churndata)
summary(mod1)
anova(mod0, mod1, test="LRT")

# Remove Marital
mod2 <- lm(MonthlyCharge ~ Tenure+Age+Children+Bandwidth_GB_Year+Churn+Gender+Contract+Area+Multiple+InternetService+StreamingTV+StreamingMovies+PaymentMethod+TechSupport+OnlineSecurity+OnlineBackup+DeviceProtection, data = churndata)
summary(mod2)
anova(mod1, mod2, test="LRT")

# Remove PaymentMethod
mod3 <- lm(MonthlyCharge ~ Tenure+Age+Children+Bandwidth_GB_Year+Churn+Gender+Contract+Area+Multiple+InternetService+StreamingTV+StreamingMovies+TechSupport+OnlineSecurity+OnlineBackup+DeviceProtection, data = churndata)
summary(mod3)
anova(mod2, mod3, test="LRT")

# Remove Area
mod4 <- lm(MonthlyCharge ~ Tenure+Age+Children+Bandwidth_GB_Year+Churn+Gender+Contract+Multiple+InternetService+StreamingTV+StreamingMovies+TechSupport+OnlineSecurity+OnlineBackup+DeviceProtection, data = churndata)
summary(mod4)
anova(mod3, mod4, test="LRT")
