#' ---
#' title: "R Data files, plot and modeling"
#' author: "Biao Zhang"
#' date: "Sep 26, 2021"
#' ---


###' 
###' #### Visualization
###' 

###' 1. (3 points) As you have seen in the in-class practice problems for visualization, the lines that connects all data points may be too wiggly (even when there is no cross lines) to reveal the overall pattern of y vs x relationship.
###' When we do not know the true relationship, we do want to explore what it is through a scatter plot,
###' ideally with as few assumptions on the form of this relationship  (e.g., quadratic, monotone, and etc) as possible.
###' One such weak assumption is smoothness. That is, the true relationship may be a smooth curve.
###' During exploratory data analysis, it is a common practice to add smooth curves to the scatter plot to visually examine the pattern.
###' R function lowess(x,y,f) generate such smooth relationship, where f is the smooth span parameter (proportion of the points that influence each data point).
###' Larger f gives smoother curve (what is the smoothest curve? and what is the least smooth curve for given data points).
###' Produce a scatter plot of `y` versus `x` (as in the lecture), and set the title and axes labels as you see fit. Then overlay on top four outputs of lowess with various smoothing span
###' using lines(lowess(x,y,f=ff),....) where ff=1,2/3,1/3,0.01, respectively. Please fill "...." with appropriate options for lines() so that the four curves have different line type, color, and width.
###' Is your output consistent with your understanding the meaning of f?
###' In particular, can you describe what type of lines lowess returns when f=1 and f=0.01, respectively. Can you explain why in terms of the meaning of f, and our data (recall that 1/n > 0.01).
###' Then add a legend to the top left corner of the plot using legend(). The legend should display the text "f=1", "f=2/3", "f=1/3", "f=0.01",
###' with corresponding symbols (line type, width and color that you chose for your lines).
n = 50
set.seed(0)
x = sort(runif(n, min=-2, max=2))
y = x^3 + rnorm(n)
plot(x, y, main = "A noisy cubic", xlab = "My x variable", ylab = "My y variable")
lines(lowess(x, y, f = 1), lty = 1)
lines(lowess(x, y, f = 2/3), lty = 2, lwd = 1.5)
lines(lowess(x, y, f = 1/3), lty = 3, lwd = 2)
lines(lowess(x, y, f = .01), lty = 4, lwd = 3)
legend("topleft", legend = c("f=1", "f=2/3", "f=1/3","f=0.01"), lty = c(1:4), lwd = c(1,1.5,2,3), cex = .65)
###' Comments: As f is the smoother span, when its value increases, the proportion of points influencing the smooth at each value increases.
###' As 1/n = .02, if we set f = .01 < .02, each point influences the smooth of the plot, we could see the line goes through all points.
###' If we set f = 1 >> .02, the line will not go through all points, it becomes much smoother.
###' When f = 1, it gives the smoothest curve, almost a straight line. When f = .01, it gives the least smooth cuerve, a curve going through all points.
###' 
###' Comments: I do not use color to differentiate the type of lines. They are all black.
###' 
###' 2. (4 points)Plot a round smiley face with size 10 "*" shaped brown eyes, and red mouth.
###' Feel free to add any other interesting features as you like such as pointy teeth, nose, ears and etc, as long as they are not offensive to the others.
x0 = (0:200-100)/100
y0 = sqrt(1-x0^2)
center.x=0
center.y=0
radius=2
plot(center.x,center.y,type='n',xlim = c(-radius,radius)+center.x,ylim = center.y+c(-radius,radius),xlab = '',ylab = '')
lines(center.x+x0*radius,center.y+y0*radius)
lines(center.x+x0*radius,center.y-y0*radius)
center.eyes.x = c(-1,1)*radius/3+center.x
center.eyes.y = radius*1/2+center.y
radius.eyes = radius*0.2
lines(center.eyes.x[1]+x0*radius.eyes,center.eyes.y+y0*radius.eyes)
lines(center.eyes.x[1]+x0*radius.eyes,center.eyes.y-y0*radius.eyes)
lines(center.eyes.x[2]+x0*radius.eyes,center.eyes.y+y0*radius.eyes)
lines(center.eyes.x[2]+x0*radius.eyes,center.eyes.y-y0*radius.eyes)
points(center.eyes.x,rep(center.eyes.y,2),pch='*',cex=10,col='brown')
center.mouth.x = center.x
center.mouth.y = -radius*0.1+center.y
radius.mouth = radius*0.6
lines(center.mouth.x+x0*radius.mouth,center.mouth.y-y0*radius.mouth, col='red')
points(center.x, center.y, cex = 8, pch = 16, col='red')

###'
###' #### Statistical Modeling
###'
###' 1. (1 point) download the zipped data for ISLR book from https://www.statlearning.com/s/ALL-CSV-FILES.zip
###' extract the zipped and load the Hitters dataset using read.csv, and call it hitters.df
###' This dataset contains information of 322 MLB players from 1986-1987.
###' You can find more context of the dataset by reading the corresponding section of https://cran.r-project.org/web/packages/ISLR/ISLR.pdf
###' Note that you can access the same dataset by installing R package ISLR, which contains all datasets using in that book.
###' Then you can also access the help package of Hitters data to learn more details.
###' But I want you to practice loading data from the actual files, because this is more common in practice.
hitters.df = read.csv("H:/MATH796/Week 4/ALL CSV FILES/Hitters.csv", header = T, quote = "\"")
###' 2. (1 point) Read the car medical charge dataset from https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv
###' Let us call it insurance.df
###' You can read https://www.kaggle.com/mirichoi0218/insurance to learn more about the dataset.
insurance.df = read.csv("https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv", header = T, quote = "\"")
###' 3. (2 points) Note that There are columns representing the cumulative statistics for the whole career of each player (CAtBat, CHits, CHmRun, CRuns, CRBI, CWalks),
###'  which naturally increase with years. We will add new columns to the data frame corresponding to the career avareges.
###'  For example, define a new column AveAtBat representing the yearly average number of times at bat during his career. You can calculate it by CAtBat devided by Years.
###'  Similarly, please define columns AveHits, AveHmRun, AveRuns, AveRBI, AveWalks.
hitters.df$AveAtBat = hitters.df$CAtBat/hitters.df$Years
hitters.df$AveHits = hitters.df$CHits/hitters.df$Years
hitters.df$AveHmRun = hitters.df$CHmRun/hitters.df$Years
hitters.df$AveRuns = hitters.df$CRuns/hitters.df$Years
hitters.df$AveRBI = hitters.df$CRBI/hitters.df$Years
hitters.df$AveWalks = hitters.df$CWalks/hitters.df$Years
###' 4. (1 point) Check the data type of each column of hitters.df
sapply(hitters.df, typeof)
###' 5. (1 point) Count the number of missing values for each column. Which variables contain missing values?
###' Remove the rows with any missing values, and refer to reduced data frame as hitters.noNA.df
###' In the following, we will analyze this data frame, and focus on predicting the salary.
count.na.col = sapply(hitters.df, function(x){sum(is.na(x))})
count.na.col
names(count.na.col[count.na.col != 0])
hitters.noNA.df = hitters.df[which(rowSums(is.na(hitters.df)) == 0),]
any(is.na(hitters.noNA.df))
###' 6. (2 points) To investigate the marginal distribution of each variable, plot the histogram for each numeric variables,
###' and barplot for each categorical variable in the datset.
par(mfrow = c(2,4), mar = c(4,4,2,.5))
for (i in c(1:ncol(hitters.noNA.df))) {
  if (typeof(hitters.noNA.df[,i]) == "character"){
    barplot(table(hitters.noNA.df[,i]), xlab = colnames(hitters.noNA.df)[i],
            main = paste("Barplot of", colnames(hitters.noNA.df)[i]))
  } else {
    hist(hitters.noNA.df[,i], xlab = colnames(hitters.noNA.df)[i],
         main = paste("Histogram of", colnames(hitters.noNA.df)[i]))
  }
}

###' 7. (2 points) To investigate the association between the Salary and each of the other numeric variables, draw a scatter plot using "Salary" at y-axis, and each of the other numeric variable at x axis,
###' For the scatter plots, you should also add a smooth line to explore the potential pattern. Please also calculate the correlation of each numeric variable and salary
par(mfrow = c(2,4), mar = c(4,4,2,.5))
col.noNA.type = sapply(hitters.noNA.df, typeof)
for (i in which(colnames(hitters.noNA.df) != "Salary" & col.noNA.type != "character")) {
  plot(hitters.noNA.df[,i], hitters.noNA.df$Salary, xlab = colnames(hitters.noNA.df)[i],
      ylab = "Salary", main = paste("Salary vs", colnames(hitters.noNA.df)[i]))
  lines(lowess(hitters.noNA.df[,i], hitters.noNA.df$Salary, f = 1), col = 'red')
}
###'
hitters.Sal.cor = cor(hitters.noNA.df$Salary, hitters.noNA.df[,which(colnames(hitters.noNA.df) != "Salary" & col.noNA.type != "character")])
hitters.Sal.cor
###' 8. (1 points) Re-do the analysis for the above question, except that now using log of Salary.
par(mfrow = c(2,4), mar = c(4,4,2,.5))
for (i in which(colnames(hitters.noNA.df) != "Salary" & col.noNA.type != "character")) {
  plot(hitters.noNA.df[,i], log(hitters.noNA.df$Salary), xlab = colnames(hitters.noNA.df)[i],
       ylab = "log(Salary)", main = paste("log(Salary) vs", colnames(hitters.noNA.df)[i]))
  lines(lowess(hitters.noNA.df[,i], log(hitters.noNA.df$Salary), f = 1), col = 'red')
}
###'
hitters.noNA.df$LogSalary = log(hitters.noNA.df$Salary)
col.noNA.addLog.type = c(col.noNA.type, "double")
hitters.logSal.cor = cor(hitters.noNA.df$LogSalary, hitters.noNA.df[,which(col.noNA.addLog.type != "character" & !(colnames(hitters.noNA.df) %in% c("Salary", "LogSalary")))])
hitters.logSal.cor
hitters.noNA.df$LogSalary = NULL

###' 9. (2 points) To investigate the association be a side-by-side box plot of "Salary" using each of the categorical variable as the grouping variable.
par(mfrow = c(1,3), mar = c(4,4,2,.5))
for (i in which(colnames(hitters.noNA.df) != "Salary" & col.noNA.type == "character")) {
  boxplot(hitters.noNA.df$Salary~factor(hitters.noNA.df[,i]), xlab = colnames(hitters.noNA.df)[i],
          ylab = "Salary", main = paste("Salary vs", colnames(hitters.noNA.df)[i]))

}

###' 10. (1 point) Test the difference in the average salary between East and West devision.
t.test(hitters.noNA.df$Salary[hitters.noNA.df$Division=="E"],hitters.noNA.df$Salary[hitters.noNA.df$Division=="W"])
###' 11. (2 points) Compute, using `lm()`, a linear regression model of the log of Salary  on Hits,AveHits,Years,Division. Save the results as `hitters.lm`.
###' Using `summary()`, display the coefficients (intercept and slope).
hitters.lm = lm(log(Salary) ~ Hits + AveHits + Years + Division, data = hitters.noNA.df)
summary(hitters.lm)
###' 12. (2 points) Plot the diagnostic plots of hitters.lm. Do you think the fit is good?
par(mfrow = c(2,2), mar = c(4,4,2,.5))
plot(hitters.lm)

###' Comments: I think generally it is good.
###' Residuals vs Fitted: It appears a small curve pattern, but generally it is randomly scattered, 218 and 296 might be outliers.
###' Normal Q-Q: Most points are along the 45 degree line, although on two tails there are some points far away from the line.
###' Scale-Location plot: No obvious group, only two points (218 and 296) are far from the center.
###' Residuals vs Leverage: All points are in Cook's distance, no potential outliers are found.
###'
###' 13. (1 point)Predict the salary at the original scale for all the players whose salary is missing from the original data.
hitters.NA.df = hitters.df[which(rowSums(is.na(hitters.df)) != 0),]
exp(predict(hitters.lm, newdata = hitters.NA.df))
###' Remark: I often use log transformation on a variable if it is always non-negative and right skewed.
###' The problem with transformation in general is that the models built using transformed variables may become hard to interpret.
###' I personally think log transformation is ok in the scenarios when I am using it.
###' But some more strict statisticians may prefer not to, and dislike what I do.
###' Similarly, sometimes I think some other statisticians are using too many different transformations of variables too arbitrarily, and too liberally.
###' What I am trying to say is that even though I have used log transformation many times in this course,
###' please do NOT take it for granted, and please do NOT take any transformation of variables for granted, even standardization
###' (linear transformation: subtracting the mean and then dividing by the standard deviation)
###' 
###' 14. (2 points) Linear model can be generalized in many ways. If the response variable does not follow a normal distribution, especially when it is discrete or categorical,
###'  you may want to try generalized linear model. When the response variable is binary (0 or 1), you can fit a logistic regression.
###' Fit a logistic regression model, using `glm()` with `family="binomial"`, with the response variable being the indicator that Salary is larger than 750,
###'  and the predictor variables being Hits, AveHits, Years, Division. Call the result `hitters.glm`.
###'  Note: you can set this up in two different ways: (i) you can manually define a new column (say) salary.high in the `hitters.noNA.df` data frame to be the indicator
###'  that the Salary column is larger than 750; or (ii) you can define an indicator variable "on-the-fly" in the call to `glm()`
###'  with an appropriate usage of `I()`. Display a summary, reporting the coefficient estimates and standard errors, and associated p-values.
hitters.noNA.df$salary.high = ifelse(hitters.noNA.df$Salary > 750, 1, 0)
hitters.glm = glm(salary.high ~ Hits + AveHits + Years + Division, data = hitters.noNA.df, family = "binomial")
summary(hitters.glm)
hitters.noNA.df$salary.high = NULL
###'  15. (4 points) Another generalization of the (generalized) linear model is the (generalized) additive modeling (GAM)
###'  GAM allows the relationship for certain predictors to be nonlinear, but a smooth function.
###'  In this example, we will use it to model Years and Hits, but not AveHits.
###'  Recall that in the scatter plot, Years obviously has a nonlinear relationship with Salary (even after log transformation).
###'   
###'   Install the `mgcv` package, if you haven't already, and load it into your R session with `library(mgcv)`.
###'    Fit a generalized additive model, using `gam()` with `family="gaussian"`, with the response variable being the log of Salary, and the predictor variables being
###'  Hits,AveHits,Years, and Division. Also, in the call to `gam()`, allow for Hits,Years to have a nonlinear effect by using `s()`.
###'  The variables that are left alone (AveHits and Dvision) will have the default, linear effects. Call the result hitters.gam. Display a summary with `summary()`.
###'   How have these predictors change, in terms of their p-value, to what you saw in the linear regression model?
###'   Also, plot the fitted effect for each nonlinear predictor, using `plot()`. Comment on the plots---does the fitted effect make sense to you? In particular,
###'    is there a strong nonlinearity associated with the effect of Years, and does this make sense?
###'    Please also predict the salary of the players who have this information missing in the original dataset.
library(mgcv)
hitters.noNA.df$LogSalary = log(hitters.noNA.df$Salary)
hitters.gam = gam(LogSalary ~ s(Hits) + AveHits + s(Years) + Division, data = hitters.noNA.df, family = "gaussian")
summary(hitters.gam)
###' In terms of p-value, they become extremely lower than the previous p-value from linear regression (Except for the predictor Deivision).
par(mfrow = c(1,2), mar = c(4,4,2,.5))
plot(hitters.gam)

###' According to the plot of the fitted effect of years, there are some nonlinear effects, we could see the curve on two sides.
###' However, about the plot of the fitted effect of Hits, although there are curves on two sides, the middle of the line are 
###' almost flat, and a lot of observations are concentrated there. There are only few observations on two sides, they may cause
###' those boundary variations. Generally speaking, further studies are required for this case.
exp(predict(hitters.gam, newdata = hitters.NA.df))
###'
