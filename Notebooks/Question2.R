library(readxl)
USDataWorkbook <- read_excel("GitHub/KraksharedProjects/CuriousConquersProject/DATA/CleanedDataSets/USDataWorkbook.xlsx")
View(USDataWorkbook)

## Question Two
#### Does unemployment rate affect the correlation between police reports and prison rates?
### Stepwise Liner Regression
library(ggplot2)

d <- ggplot(USDataWorkbook, aes(x = PrisonPopulation, y = PoliceReports))
d + geom_point()+ geom_smooth(method=lm,se=FALSE,color = "red")

u <- ggplot(USDataWorkbook, aes(x = Urate, y = PoliceReports))
u + geom_point()+ geom_smooth(method=lm, color = "red")

u <- ggplot(USDataWorkbook, aes(x = Urate, y = PrisonPopulation))
u + geom_point()+ geom_smooth(method=lm, color = "red")

cor.test(USDataWorkbook$Urate, USDataWorkbook$PrisonPopulation, method="pearson", use = "complete.obs")
#Not signifcant .13
cor.test(USDataWorkbook$Urate, USDataWorkbook$PoliceReports, method="pearson", use = "complete.obs")
#Not signifcant .29
cor.test(USDataWorkbook$PrisonPopulation, USDataWorkbook$PoliceReports, method="pearson", use = "complete.obs")
#Significant .69

#install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")

data_quant <- USDataWorkbook[, c(2,3,4,5,6,7,8,9,10)]
chart.Correlation(data_quant, histogram=FALSE, method="pearson")
ques_quant <- USDataWorkbook[, c(2,5,8)]
chart.Correlation(ques_quant, histogram=FALSE, method="pearson")

corr_matrix <- cor(data_quant)
corr_matrix

corr_matrix <- cor(ques_quant)
corr_matrix

#install.packages("corrplot")
library("corrplot")

corrplot(corr_matrix, type="upper", order="hclust", p.mat = corr_matrix, sig.level = 0.01, insig="blank")

lin_reg <- lm(PrisonPopulation ~ PoliceReports, USDataWorkbook)
print(lin_reg)

#The equation line is y= 5.187e-03x-7.439e+03

summary(lin_reg)

#given police reports we can significantly predict how many prisoners per state
#explaining 47% of the data

FitAll = lm(Urate ~ ., data = data_quant)
summary(FitAll)

#not a good fit
#preform backwards elimination

step(FitAll, direction = 'backward')

#best predictor of Unemployment rate is Population and Prison Population
#preform forwards elimination
fitstart = lm(Urate ~ 1, data = data_quant)

step(fitstart, direction = 'forward', scope = (formula(FitAll)))
#same conclusion

Quest2=lm(formula = Urate ~ PoliceReports + PrisonPopulation, data = data_quant)
summary(Quest2)
#Unemployment rate is slightly significant relationship on Prison Population but not on Police reports

---
#Better question
  #How is Prison Population relation to the other data
  FitAll = lm(PrisonPopulation ~ ., data = data_quant)
summary(FitAll)


#significant fit 
#explains 90% of prison population

#preform backwards elimination

step(FitAll, direction = 'backward')

lm(formula = PrisonPopulation ~ `Pop. 2021` + PoliceReports + 
ViolentCrime + PropertyCrime + PropertyPrison, data = data_quant)

#preform forwards elimination
fitstart = lm(PrisonPopulation ~ 1, data = data_quant)

step(fitstart, direction = 'forward', scope = (formula(FitAll)))
#
lm(formula = PrisonPopulation ~ `Pop. 2021` + PoliceReports + 
     PropertyPrison, data = data_quant)