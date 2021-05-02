# This analysis is for understanding the significance of categorical variables, to make determination of what should be included in the model
# 1.	Read data from the CSV file and rename the relevant columns
churndata <- read.csv("/Users/ASHA15/Desktop/C/Learn/MS_DataAnalytics_WGU/Term2/D208_PredictiveModeling/Task1/churn_clean.csv")
names(churndata)[names(churndata) == "Item1"] <- "Timely_response"
names(churndata)[names(churndata) == "Item3"] <- "Timely_replacements"

# 2.	Create all dummy variables or factors as they are called in R notation

churndata\$State <- factor(churndata\$State)
churndata\$Churn <- factor(churndata\$Churn)
churndata\$Marital <- factor(churndata\$Marital)
churndata\$Gender <- factor(churndata\$Gender)
churndata\$Techie <- factor(churndata\$Techie)
churndata\$Contract <- factor(churndata\$Contract)
churndata\$Port_modem <- factor(churndata\$Port_modem)
churndata\$Tablet <- factor(churndata\$Tablet)
churndata\$InternetService <- factor(churndata\$InternetService)
churndata\$Phone <- factor(churndata\$Phone)
churndata\$Multiple <- factor(churndata\$Multiple)
churndata\$TechSupport <- factor(churndata\$TechSupport)
churndata\$StreamingTV <- factor(churndata\$StreamingTV)
churndata\$StreamingMovies <- factor(churndata\$StreamingMovies)
churndata\$PaperlessBilling <- factor(churndata\$PaperlessBilling)
churndata\$PaymentMethod <- factor(churndata\$PaymentMethod)


# 3.	Create model with different columns and next model with one column removed, repeatedly, and keep the column removed if likelihood ration test show high p-value (anything above 0.25)

mod0 <- glm(Churn ~ Tenure+Contacts+MonthlyCharge+Bandwidth_GB_Year+State+Marital+Gender+Techie
            +Contract+Age+Children+Port_modem+Multiple+InternetService+Phone+Timely_response+Timely_replacements+
              StreamingMovies+PaperlessBilling+PaymentMethod, data = churndata, family = "binomial")
summary(mod0)

# 4.	Remove State
mod1 <- glm(Churn ~ Tenure+Contacts+MonthlyCharge+Bandwidth_GB_Year+Marital+Gender+Techie+
              Contract+Age+Children+Port_modem+Multiple+InternetService+Phone+Timely_response+Timely_replacements+
              StreamingMovies+PaperlessBilling+PaymentMethod, data = churndata, family = "binomial")
summary(mod1)
anova(mod0, mod1, test="LRT")

# 5.	Remove Timely_response
mod2 <- glm(Churn ~ Tenure+Contacts+MonthlyCharge+Bandwidth_GB_Year+Marital+Gender+Techie
            +Contract+Age+Children+Port_modem+Multiple+InternetService+Phone+Timely_replacements
            +StreamingMovies+PaperlessBilling+PaymentMethod, data = churndata, family = "binomial")
summary(mod2)
anova(mod1, mod2, test="LRT")

# 6.	Remove Timely_replacements
mod3 <- glm(Churn ~ Tenure+Contacts+MonthlyCharge+Bandwidth_GB_Year+Marital+Gender+Techie+Contract+Age+Children+Port_modem+Multiple+InternetService+Phone+StreamingMovies+PaperlessBilling+PaymentMethod, data = churndata, family = "binomial")
summary(mod3) anova(mod2, mod3, test="LRT")
# 7.	Remove Multiple
mod4 <- glm(Churn ~ Tenure+Contacts+MonthlyCharge+Bandwidth_GB_Year+Marital+Gender+Techie
            +Contract+Age+Children+Port_modem+InternetService+Phone+StreamingMovies+PaperlessBilling
            +PaymentMethod, data = churndata, family = "binomial")
summary(mod4)
anova(mod3, mod4, test="LRT")

# 8.	Remove Gender
mod5 <- glm(Churn ~ Tenure+Contacts+MonthlyCharge+Bandwidth_GB_Year+Marital+Techie
            +Contract+Age+Children+Port_modem+InternetService+Phone+StreamingMovies
            +PaperlessBilling+PaymentMethod, data = churndata, family = "binomial")
summary(mod5)
anova(mod4, mod5, test="LRT")
