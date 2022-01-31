#' ---
#' title: "R Basics Lab"
#' author: "Qi Zhang (including materials from Dr. Ryan Tibshirani's class at CMU)"
#' date: "September 3, 2021"
#' ---
#' 


###' 
###' Vector, manipulation, indexing
###' 
###'
###' 
###' 1. Please write R program to answer each of the following questions. (1 point each)
###'  We will use an R built-in dataset discovery that present the  numbers of "great" inventions and scientific discoveries in each year from 1860 to 1959.
###' You can get the dataset using command data(discoveries). Please answer each of the following question using no more than three lines of R code, except part g) for which no more than 10 lines.
###' You may find it useful to first create a variable named year whose values are the corresponding years of discoveries. 
###' a) Display the first 5 elements of this vector
###' b) Compare the total numbers of great discoveries during WWI (1914-1918) and WWII(1939-1945), during which war there are more discoveries? 
###' c) Find the year with the most great discoveries after 1930
###' d) Find the years with no discoveries.
###' e) Find the year with the second most discoveries in this 100 years
###' f) Which is bigger, the mean or median? Without a plot, can you guess the skewness of the distribution?
###' g) Function summary() produce the five number summaries and the mean for a vector, please use the output of this function to identify outliers in this vector.
###'
###' Solution:
data("discoveries")
year = as.numeric(time(discoveries))
names(discoveries) = year
###' a)
head(discoveries,5)
###' b) There are more discoveries in WWI.
sum(discoveries[year <= 1918 & year >= 1914]) > sum(discoveries[year <= 1945 & year >= 1939])
###' c) They are years 1939, 1947, 1948 and 1952, all have 4 discoveries.
dis_aft1930 = discoveries[year > 1930] 
dis_aft1930[dis_aft1930 == max(dis_aft1930)]
###' d) They are the year 1962, 1864, 1881, 1904, 1917, 1933, 1956, 1957 and 1959.
names(discoveries[discoveries == 0])
###' e) It is the year 1887.
names(sort(discoveries,decreasing = T)[2])
###' f) Since the mean is greater than the median. It is right skewed.
mean(discoveries) > median(discoveries)
###' g) Outliers are the year 1885, 1887, 1888, 1913.
dis_stats = as.vector(summary(discoveries))
names(dis_stats) = names(summary(discoveries))
dis_Range = (dis_stats["3rd Qu."] - dis_stats["1st Qu."]) * 1.5
names(dis_Range) = NULL
discoveries[discoveries < (dis_stats["1st Qu."] - dis_Range) | discoveries > (dis_stats["3rd Qu."] + dis_Range)]

###' 
###' Matrix manipulation
###' 

###' 2. Please write R program to answer each of the following questions (1 point each except f, 2 points for f)
###' a) Generate 300 binomial random samples, composed of 15 trials each, with probability of success to be 0.2, 0.4, 0.6, and 0.8
###'  storing the results in vectors called `bin.draws.0.2`,  `bin.draws.0.4.`, `bin.draws.0.6`,  and  `bin.draws.0.8`. 
###' b) Create means.bin.draws, a vector with length 4, whose elements are the means of these four vectors.
###' c) Create a matrix of dimension 300 x 4, called `bin.matrix`, whose columns contain the 4 vectors we've created, 
###' in order of the success probabilities of their underlying binomial distributions (0.2 through 0.8). Hint: use `cbind()`. 
###' d) Print the first five rows of `bin.matrix`. Print the element in the 66th row and 3rd column. 
###' Compute the largest element in first column. Compute the largest element in all but the first column.
###' e) Calculate the column means of `bin.matrix` by using just a single function call.
###' f) Compare the means you computed in the last question to those you computed in b), in two ways. 
###' First, using `==`, and second, using `identical()`. What do the two ways report? Are the results compatible? Explain.
###' 
###' Solution:

###' a.
bin.draws.0.2 = rbinom(300, 15, .2)
bin.draws.0.4 = rbinom(300, 15, .4)
bin.draws.0.6 = rbinom(300, 15, .6)
bin.draws.0.8 = rbinom(300, 15, .8)
###' b)
means.bin.draws = c(mean(bin.draws.0.2), mean(bin.draws.0.4), mean(bin.draws.0.6), mean(bin.draws.0.8))
names(means.bin.draws) = c("bin.draws.0.2","bin.draws.0.4","bin.draws.0.6","bin.draws.0.8")
###' c)
bin.matrix = cbind(bin.draws.0.2,bin.draws.0.4,bin.draws.0.6,bin.draws.0.8)
###' d)
bin.matrix[c(1:5),]
bin.matrix[66,3]
max(bin.matrix[,1])
max(bin.matrix[,-1])
###' e)
colMeans(bin.matrix)
###' f) `==` returns a vector of TRUE, it tells the equilty in each index.`identical()` returns a signgle value, it tells whether two objects are identical.
###' In this case, those two results are compatible. The element in each index is equivalent iff the object are identical.
colMeans(bin.matrix) == means.bin.draws
identical(colMeans(bin.matrix), means.bin.draws)

###'  
###' Data frame
###' 
###' 
###' 3. Dataset beavers include the data for two beavers from a study described in Reynolds (1994). (1 point each except h, 2 points for h)
###' Reynolds (1994) describes a small part of a study of the long-term temperature dynamics of beaver Castor canadensis in north-central Wisconsin. 
###' Body temperature was measured by telemetry every 10 minutes for four females, but data from a one period of less than a day for each of two animals is used there. 
###' You can see https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/beavers.html for more details of the dataset.
###' You can load the data with command data(beavers), and then two data frames (beaver1 and beaver2) are included in the environment.
###' Please then define a list beavers whose two elements are the data for each beaver (beaver1 and beaver2)
###' Please answer each of the following questions with no more than five lines of R code, except part h, no more than 15 lines.
###' a) Calculate the average temperatures of beaver1 when it has activities outside the retreat (activ is 1).
###' b) what is the proportion of the time points that beaver2's body temperature is between 37.00 and 38.00?
###' c) what is the proportion of the time points between 18:00 and 23:00 that beaver2's body temperature is between 37.00 and 38.00?
###' d) Compare the body temperatures of the two beavers at 1200
###' e) Using one line of R code, calculating the averages for columns temp and activ of beaver1.
###' f) The column temp is in Celsius, it can be converted to Fahrenheit by 1.8*Celsius+32, add a column tempF to beaver1 and beaver2 that present the body temperature in Fahrenheit. 
###' g) Starting from the list beavers, display the 10th and 13th row of the data for beaver2.
###' h) Define a list beaver.temp.afternoon with two elements with names beaver1 and beaver2. 
###' The two elements are the vectors of the body temperature for beaver1 and beaver2 between 12:00pm and 6:00pm, 
###' and the elements of these vectors are named with their corresponding time. 
###' Display the first five elements of each element of the list you created to make sure that it is consistent with your expectations. 
###' 
###' Solution:
data(beavers)
beavers.list = list(beaver1 = beaver1, beaver2 = beaver2)
###' a)
mean(beaver1[beaver1$activ == 1, "temp"])
###' b)
length(beaver2[beaver2$temp >= 37.00 & beaver2$temp <= 38.00, "time"])/length(beaver2[,"time"])
###' c)
beaver2.sub = subset(beaver2, time >= 1800 & time <= 2300)
length(beaver2.sub[beaver2.sub$temp >= 37.00 & beaver2.sub$temp <= 38.00, "time"])/length(beaver2.sub[,"time"])
###' d) beaver2's temperature is higher than beaver1s' at 12:00 p.m.
beaver1[beaver1$time == 1200, "temp"] >= beaver2[beaver2$time == 1200, "temp"] 
###' e)
colMeans(beaver1[,c("temp","activ")])
###' f)
beaver1$tempF = beaver1$temp*1.8 + 32
beaver2$tempF = beaver2$temp*1.8 + 32
###' g)
beavers.list$beaver2[c(10,13),]
###' h)
beaver1.v = beaver1[beaver1$time >= 1200 & beaver1$time <= 1800,"temp"]
names(beaver1.v) = beaver1[beaver1$time >= 1200 & beaver1$time <= 1800,"time"]
beaver2.v = beaver2[beaver2$time >= 1200 & beaver2$time <= 1800,"temp"]
names(beaver2.v) = beaver2[beaver2$time >= 1200 & beaver2$time <= 1800,"time"]
beaver.temp.afternoon = list(beaver1 = beaver1.v, beaver2 = beaver2.v) 
beaver.temp.afternoon$beaver1[c(1:5)]
beaver.temp.afternoon$beaver2[c(1:5)]
###' 