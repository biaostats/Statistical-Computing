#' ---
#' title: "R Data files, plot and modeling"
#' author: "Qi Zhang (including materials from Dr. Ryan Tibshirani's class at CMU)"
#' date: "August 01, 2021"
#' ---


###'
###' #### Merging data
###'Suppose you have two data frames X, Y, and you want to combine them.
###'How you will do it depends on what you want to combine, and there are several cases.
###'
###' * (1) Simplest case: the rows of the data frames represent exactly the same units, in the same order. You want all columns from both. Then you just use, data.frame(X,Y)
###' * (2) The rows of the data frames represent exactly the same units, in the same order. But you only want some columns from both.
###'Let colX and colY represent the vectors of column names of the columns of X and Y that you want to combine, then you just use data.frame(X[,colX],Y[,colY])
###' * (3) The rows of the data frames represent exactly the same units, but in different order. Put them in the same order, with order(). Alternatively, use merge()
###' * (4) The hard case: there are units in common, but also unique units to X or Y. It would be hard to line up.
###'
###' You need to decide whether you only want the units that are in common in both data frames, or also want to keep the units unique to X or Y.
###' If the latter, you also need to decide how to present the missing values.
###'
###'Example: County level Covid data
###'
###' Suppose we are interested in investigating how the demographics of a county in US affect their COVID cases and death on a given date (say, June 3rd 2021).
###'
###' NY Times has county level covid data, but no information about the demographics in the counties.
###' Census.org has demographics data, but not covid cases.
###' We need to merge these datasets to perform any analysis.
###'
###' In this example, you will see some technics in text manipulation and data cleaning.
###' While they are useful in practice, you are not required to know how to use them for this course.
###' However, when you are merging data in practice, similar issues may arise.

###' We first merge the two Census datasets.

dir.data = "D:\\Dropbox\\courses\\stat_computing\\data\\"
setwd(dir.data)
census2018dp02 <- read.csv('ACSDP5Y2018.DP02_data_with_overlays_2020-06-12T230404.csv',header=T,skip=1)
census2018dp05 <- read.csv('ACSDP5Y2018.DP05_data_with_overlays_2020-06-12T095532.csv',header=T,skip=1)
dim(census2018dp02)
dim(census2018dp05)
census2018dp02[1,1:5]
census2018dp05[1,1:5]

identical(census2018dp02$Geographic.Area.Name,census2018dp05$Geographic.Area.Name)

###' The rows in the two Census datasets represent the same units in the same order.
census2018 <- data.frame(census2018dp02,census2018dp05[,-(1:2)])
dim(census2018)
census2018[1,1:5]
tail(census2018,1)[1:10]

###' We remove the columns for the Margin of Error of the Census estimates, and Puerto Rico for simplicity of the example.
ix.me <- grepl('Margin.of.Error',names(census2018))
sum(ix.me)
census2018.pe <- census2018[!grepl('Puerto Rico',census2018$Geographic.Area.Name),!ix.me]
dim(census2018.pe)

###' The covid dataset from NY Times
setwd(dir.data)
covid <- read.csv('us-counties-recent.csv')
dim(covid)
head(covid)
tail(covid)
###' The information of each county on each of the last 30 days are recorded. We will focus on the information on a particular date, and remove Virgin Islands, Northern Mariana Islands and Puerto Rico for simplicity.
covid0603 <- covid[covid$date=='2021-06-03',]
dim(covid0603)
unique(covid0603$state)
###' Remove the data with unknown county, and the data for Virgin Islands, Northern Mariana Islands and Puerto Rico.
covid0603.county <- covid0603[covid0603$county!='Unknown'&!is.element(covid0603$state,c('Virgin Islands','Northern Mariana Islands','Puerto Rico')),]
dim(covid0603.county)

###' The rows of the covid data and the census data represent mostly the same units, potentially with different order.
###'
###' But there must be some differences in the units that the row represents, because they have different numbers of rows.
###'
###' Let us find out these differences during the following process of building a common variable that matches the rows of the two data frames.
###'
###' We need to find a variable that is common to the two data frames, and is able to uniquely identify the rows in each column.
###'
###' e.g., if this column is called "key", and X\$key[2] == Y\$key[5], then the 2nd row of X and the 5th row of Y represent the same unit(subject).
###'
###' One choice of such could be a common variable (by nature but not necessarily by name) that has a unique value for each unit.
###'
###' When such key is unavailable directly, we can define one.
id.region.covid <-  paste(covid0603.county$county,covid0603.county$state,sep='_')
id.region.census <- gsub(' County, ','_',census2018.pe$Geographic.Area.Name)

head(id.region.covid)
head(id.region.census)
###' Let us see how many matches we have found.
length(intersect(id.region.covid,id.region.census))
###' Let us investigate the differences.
###' setdiff(A,B) outputs the values that are in vector A, but not B
id.census.only <- sort(setdiff(id.region.census,id.region.covid))
id.covid.only <- sort(setdiff(id.region.covid,id.region.census))
length(id.census.only)
length(id.covid.only)
head(id.census.only,10)
head(id.covid.only,10)

###' You may realized that many of them because some counties are not called XX County. So the true separator between county name and the sate name is ", " instead of " County,"
###' We will substitute ", " with "_", and remove words such as "County", "Parish", and white space.
id.region.covid <-  gsub(' ','',gsub('Municipality','',paste(covid0603.county$county,covid0603.county$state,sep='_')))
id.region.census <- gsub(' ','',gsub('Municipality','',gsub('Parish','',gsub('County','',gsub(',','_',census2018.pe$Geographic.Area.Name)))))

length(intersect(id.region.covid,id.region.census))
id.census.only <- sort(setdiff(id.region.census,id.region.covid))
id.covid.only <- sort(setdiff(id.region.covid,id.region.census))
length(id.census.only)
length(id.covid.only)
#head(id.census.only)
#head(id.covid.only)
id.census.only
id.covid.only

###' We now find that most of these differences are due to difference in measurement unit: Covid data report NYC as a whole, while census data have entires for each county separately.
###' Similarly, Covid data combine parts of Alaska together. Since such discrepancy makes them incomparable, and there are not many, we may want to remove them from the merged data.
###' The county in New Mexico seems to be due to encoding error. I do not know what is happening to the two counties in Missouri,
###' because there is no potential match in census data, and every county in census data are matched by something else in the covid data.
###'
###' We may be able to save these data points if we are willing to spend more time on the context of the data.
###'
###' For example, maybe Kansas City and the county it is in produce separate reports of covid cases.
###' We may be also able to create NYC level census data that match NYC covid data by adding the county level data within NYC border.
###'
###' We need to be very careful when deciding what to delete from the dataset. Sometimes deleting data points, even just a few, may bias the downstream analysis.
###' Selection bias occurs when the data points excluded are not a random sample of all data points, and the exclusion probability confound with the analysis goal.
###'
###' For now, let us assume that we can safely remove these points, and only save the county in New Mexico.
###' What is the county in New Mexico.
###' I found on google that it is Do$\tilde{n}$a Ana, and the weired string may be due to the coding failure of $\tilde{n}$ when compiling the dataset.
id.region.census[id.region.census=="Do??????aAna_NewMexico"] <- 'DonaAna_NewMexico'
id.region.covid[id.region.covid=="Do????aAna_NewMexico"] <- 'DonaAna_NewMexico'

###' Now we add this new column to both data frames so that they ahve a common column.
#covid0603.county$region_id_x <- id.region.covid
#census2018.pe$region_id_y <- id.region.census
covid0603.county$region_id <- id.region.covid
census2018.pe$region_id <- id.region.census


###' merge() function merge two data frames, and match the rows by the argument "by".
###' If you set all=F, the units unique to each data frame will be removed. Otherwise, (some of) them will be kept.
dt.covid.census <- merge(x=covid0603.county,y=census2018.pe,by='region_id',all=F)
dim(dt.covid.census)

#dt.covid.census.2 <- merge(x=covid0603.county,y=census2018.pe,by.x='region_id_x',by.y='region_id_y',all=F)
#dim(dt.covid.census.2)


###'The merge() function tries to merge two data frames according to common columns, as in: merge(x, y, by.x="SomeXCol", by.y="SomeYCol"), to join two data frames x, y, by matching the columns ???SomeXCol??? and ???SomeYCol???
###'Default (no by.x, by.y specified) is to match all columns with common names
###'Output will be a new data frame that has all the columns of both data frames
###'
###' if all.x=T, all units unique to data frame x will be kept. These units are not in data frame y, so their values in the columns that are unique to y will be missing values.



###'
###' #### Practice
###' Note that covid.college also include the county and state it each college is in.
###'
###' For each college, we want to add the covid information of the county that it is in from covid0603.county.
###'
###' For simplicity, we will focus on the New England states  Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island, and Vermont.
###'
###' 1. You can first create a data frame covid0603.ne by extracting the rows of covid0603.county whose state is one of the New England states,
###' and covid.college.ne by extract the rows of covid.college in the New England states. What are the dimensions of these two data frames.
###'
###' 2. Then, please merge covid.college.ne and covid0603.ne. We want to keep every row in covid.college.ne. But we are not interested in the counties with no college.
###' Let us call the final output covid.college.w.county.ne. What is the dimension of this data frame? Can you guess how merge function handles it when one row of Y can be mapped to multiple rows of X?

covid.college = read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/colleges/colleges.csv',header=T)
head(covid.college)

state.ne <- c('Connecticut', 'Maine', 'Massachusetts', 'New Hampshire', 'Rhode Island', 'Vermont')
covid.college.ne <- subset(covid.college,is.element(state, state.ne))
dim(covid.college.ne)

covid0603.ne <- subset(covid0603.county,is.element(state,state.ne))
dim(covid0603.ne)

id.region.college <- gsub(' ','',paste(covid.college.ne$county,covid.college.ne$state,sep='_'))
setdiff(id.region.college,covid0603.ne$region_id)
setdiff(covid0603.ne$region_id,id.region.college)

covid.college.ne$region_id <- id.region.college
covid.college.w.county.ne <- merge(covid.college.ne,covid0603.ne,by='region_id',all.x=T,all.y=F)
dim(covid.college.w.county.ne)
tail(covid.college.w.county.ne)

###'
###' ## Visualization
###' Base R has a set of powerful plotting tools. An overview:
###'
###'   plot(): generic plotting function
###'
###' points(): add points to an existing plot
###'
###' lines(), abline(): add lines to an existing plot
###'
###' text(), legend(): add text to an existing plot
###'
###' rect(), polygon(): add shapes to an existing plot
###'
###' hist(), image(): histogram and heatmap
###'
###' heat.colors(), topo.colors(), etc: create a color vector
###'
###' density(): estimate density, which can be plotted
###'
###' contour(): draw contours, or add to existing plot
###'
###' curve(): draw a curve, or add to existing plot
###'

###' #### Scatter plot
###' To make a scatter plot of one variable versus another, use plot()

n = 50
set.seed(0)
x = sort(runif(n, min=-2, max=2))
y = x^3 + rnorm(n)
plot(x, y)


###' Plot type:
###' The type argument controls the plot type. Default is p for points; set it to l for lines

plot(x, y, type="p")


plot(x, y, type="l")

###' type argument can also be b or o, for both points and lines

###' Labels:
###' The main argument controls the title; xlab and ylab are the x and y labels

plot(x, y, main="A noisy cubic") # Note the default x and y labels


plot(x, y, main="A noisy cubic", xlab="My x variable", ylab="My y variable")


###' Point type:
###' Use the pch argument to control point type

plot(x, y, pch=21) # Empty circles, default


plot(x, y, pch=19) # Filled circles

###' For more options, see https://stat.ethz.ch/R-manual/R-patched/library/graphics/html/points.html

###' Line type:
###' Use the lty argument to control the line type, and lwd to control the line width

plot(x, y, type="l", lty=1, lwd=1) # Solid line, default width

plot(x, y, type="l", lty=2, lwd=3) # Dashed line, 3 times as thick

###' Color:
###'
###' Use the col argument to control the color. Can be:

###'   An integer between 1 and 8 for basic colors
###'
###' A string for any of the 657 available named colors
###'
###' The function colors() returns a string vector of the available colors

plot(x, y, pch=19, col=1) # Black, default

plot(x, y, pch=19, col=2) # Red

###' Multiple plots:
###'
###' To set up a plotting grid of arbitrary dimension, use the par() function, with the argument mfrow.
###'
###' Note: in general this will affect all following plots! (Except in separate R Markdown code chunks)

par(mfrow=c(2,2)) # Grid elements are filled by row
plot(x, y, main="Red cubic Points", pch=20,cex=3,col="red")
plot(x, y, main="Blue cubic Points", pch=21, col="blue")
plot(x, y, main="Green cubic Line", type = 'l',lty=1,lwd=10, col="green")
plot(x, y, main="Purple cubic Line", type='l',lty=3, lwd=1,col="purple")

par()
par(mfrow=c(1,1))
dev.off()

###' Margin
###'
###' Default margins in R are large (and ugly); to change them, use the par() function, with the argument mar.
###'
###' Note: in general this will affect all following plots! (Except in separate R Markdown code chunks ???)

par(mfrow=c(2,2), mar=c(4,4,2,0.5))
plot(x, y, main="Red cubic Points", pch=20,cex=3,col="red")
plot(x, y, main="Blue cubic Points", pch=21, col="blue")
plot(x, y, main="Green cubic Line", type = 'l',lty=1,lwd=10, col="green")
plot(x, y, main="Purple cubic Line", type='l',lty=3, lwd=1,col="purple")


###' Saving plots
###'
###' Use the pdf() function to save a pdf file of your plot, in your R working directory. Use getwd() to get the working directory, and setwd() to set it

setwd(dir.data)
getwd()
pdf(file="noisy_cubics.pdf", height=7, width=7) # Height, width are in inches
par(mfrow=c(2,2), mar=c(4,4,2,0.5))
plot(x, y, main="Red cubic Points", pch=20,cex=3,col="red")
plot(x, y, main="Blue cubic Points", pch=21, col="blue")
plot(x, y, main="Green cubic Line", type = 'l',lty=1,lwd=10, col="green")
plot(x, y, main="Purple cubic Line", type='l',lty=3, lwd=1,col="purple")
graphics.off()
###' Also, use the jpg() and png() functions to save jpg and png files



###' Adding to plots: you can draw a plot first, and then add  more components to it.
###' The main tools for this are:
###'
###'   points(): add points to an existing plot
###' lines(), abline(): add lines to an existing plot
###' text(), legend(): add text to an existing plot
###' rect(), polygon(): add shapes to an existing plot
###' You???ll get practice with this on lab. Pay attention to layers???they work just like they would if you were painting a picture by hand

plot(x, y, main="Blue cubic Points", type='n')
points(x, y)
lines(x, y, lty=1,lwd=1, col="green")

###'
###' #### Practice
###' 1. Below is some code that is very similar to that from the lecture, but with one key difference.
###' Please explain why does the `plot()` result with with `type="p"` look normal, but the `plot()` result with `type="l"` look abnormal, having crossing lines?
###'Then modify the code below (hint: modify the definition of `x`), so that the lines on the second plot do not cross.
n = 50
set.seed(0)
x = runif(n, min=-2, max=2)
y = x^3 + rnorm(n)
plot(x, y, type="p")
plot(x, y, type="l")

###' 2. The `cex` argument can used to shrink or expand the size of the points that are drawn. Its default value is 1 (no shrinking or expansion).
###'
###' Values between 0 and 1 will shrink points, and values larger than 1 will expand points.
###'
###' Plot `y` versus `x`, first with green filled circle and `cex` equal to 0.5, and then with purple empty circle and "cex" 3 (so, two separate plots).
###' Give titles "Shrunken green filled circle", and "Expanded purple empty circles", to the plots, respectively.
###'
###' 3. The `xlim` and `ylim` arugments can be used to change the limits on the x-axis and y-axis, respectively.
###' Each argument takes a vector of length 2, as in `xlim = c(-1, 0)`, to set the x limit to be from -1 to 0.
###'
###' Plot `y` versus `x`, with the x limit set to be from -1 to 1, and the y limit set to be from -5 to 5.
###' Assign x and y labels "Trimmed x" and "Trimmed y", respectively.
###'
###' 4. The `pch` argument, recall, controls the point type in the display. In the lecture examples, we set it to a single number.
###'But each of pch, col,cex can also be a vector, with one entry per point in the plot (recycle if shorter). So, e.g.,
plot(1:10, 1:10, pch=1:10,cex=1:3,col=c('red','blue'))
###'displays the first 10 point types with alternating sizes 1 to 3 and colors red and blue.
###'
###'Plot `y` versus `x`, and repeat the following pattern for the displayed points: a black empty circle, a blue filled circle, a black empty circle, a red filled circle.

###' 5. Produce a scatter plot of `y` versus `x` (as in the lecture), and set the title and axes labels as you see fit.
###' Then overlay on top a scatter plot of `y2` versus `x2`,
###' using the `points()` function, where `x2` and `y2` are as defined below.
###' In the call to `points()`, set the `pch` and `col` arguments appropriately
###'  so that the overlaid points are drawn as filled blue circles.

x2 = sort(runif(n, min=-2, max=2))
y2 = x^2 + rnorm(n)

###' 6. Starting with your solution code from the last question, overlay a line plot of `y` versus `x` on top of the plot
###'(which contains empty black circles of `y` versus `x`, and filled blue circles of `y2` versus `x2`), using the `lines()` function.
###'
###'In the call to `lines()`, set the `col` and `lwd` arguments so that the line is drawn in red, with twice the normal thickness.
###'Look carefully at your resulting plot. Does the red line pass overtop of or underneath the black empty circles?
###'What do you conclude about the way R *layers* these additions to your plot?
###'Then a blue dash line for x2, y2.
###'
###' 7. Starting with your solution code from the last question, add a legend to the bottom right corner of the the plot using `legend()`.
###'The legend should display the text: "Cubic" and "Quadratic", with corresponding symbols: an empty black circle with solid line and a filled blue circle with dash line, respectively.
###'
###'Hint: it will help to look at the documentation for `legend()`.


n = 50
set.seed(0)
x = runif(n, min=-2, max=2)
y = x^3 + rnorm(n)

order.x = order(x)
plot(x[order.x],y[order.x],type='l')


plot(x, y, type="p",cex=0.5,col='green',pch=16,main='Shrunken green filled points')
plot(x, y, type="p",cex=2,col='purple',pch=1,main='Expanded purple empty points')

plot(x, y, type="p",xlim = c(-1,1),,ylim=c(-5,5),xlab='Trimmed x',ylab='Trimmed y')


plot(x[order.x],y[order.x],pch=c(1,16),col=c('black','blue','black','red'))

n = 50
set.seed(0)
x = sort(runif(n, min=-2, max=2))
y = x^3 + rnorm(n)

plot(x,y,type='p',col='black',pch=1)

x2 = sort(runif(n, min=-2, max=2))
y2 = x^2 + rnorm(n)


points(x2,y2,col='blue',pch=19)

lines(x,y,col='black',lty=1,lwd=2)
lines(x2,y2,col='blue',lty=2,lwd=1)

legend(x='bottomright',legend=c('Cubic','Quadratic'),pch=c(1,19),lty=1:2,col=c('black','blue'),lwd=2:1)



###'
###' #### Plotting a histogram:
###' To plot a histogram of a numeric vector, use hist()
hist(y)

###' Histogram options
###' Several options are available as arguments to hist(), such as col, freq, breaks, xlab, ylab, main

hist(y, col="pink", freq=TRUE) # Frequency scale, default

hist(y, col="pink", freq=FALSE,breaks=10, xlab="y", main="Noisy cubic")

###' To add a histogram to an existing plot (say, another histogram), use hist() with add=TRUE
###'
###' We plot the histogram of investigate the distribution of the variable under consideration.
###'
###' It is a common practice to overlay a smooth curve to a histogram to to get a better sense of what the distribution may look like.
###'
###' To estimate a density from a numeric vector, use density(). This returns a list; it has components x and y, so we can actually call lines() directly on the returned object
###'
###' density() essentially estimate the density by fitting a smooth curve on top of the histogram, without assuming any parametric family.
###'

density.est = density(y) # 1.5 times the default bandwidth
class(density.est)
names(density.est)
hist(y, col="pink", freq=FALSE, breaks=10, xlab="y", main="Noisy cubic")
lines(density.est, lwd=3)


###'
###' #### Plotting a heatmap
###'
###' Plotting a heatmap with the right rotation seems to be a good practice for writing function and heatmap
###' To plot a heatmap of a numeric matrix, use image()
###' For heatmap, there are also built-in R functions heatmap, and heatmap.2 from gplots package. But their defult options are tuned towards more complext analysis.

(mat = 1:6 %o% 6:10) # %o% gives for outer product
image(mat) # Red means low, white means high
###' The orientation of image() is to plot the heatmap according to the following order, in terms of the matrix elements:
###' This is a 90 degrees counterclockwise rotation of the usual printed order for a matrix.
###'
###' Therefore, if you want the displayed heatmap to follow the usual order, you must rotate the matrix 90 degrees clockwise before passing it in to image().
###' (Equivalently: reverse the row order, then take the transpose.) Convenient way of doing so:
clockwise90 = function(a) { t(a[nrow(a):1,]) } # Handy rotate function
image(clockwise90(mat))


###' Color scale:
###' The default is to use a red-to-white color scale in image(). But the col argument can take any vector of colors.
###' Built-in functions gray.colors(), rainbow(), heat.colors(), topo.colors(), terrain.colors(), cm.colors() all return continguous color vectors of given length

phi = dnorm(seq(-2,2,length=50))
normal.mat = phi %o% phi
image(normal.mat) # Default is col=heat.colors(12)

image(normal.mat, col=heat.colors(20)) # More colors

image(normal.mat, col=terrain.colors(20)) # Terrain colors

image(normal.mat, col=topo.colors(20)) # Topological colors

###' To draw contour lines from a numeric matrix, use contour(); to add contours to an existing plot (like, a heatmap), use contour() with add=TRUE

contour(normal.mat)

image(normal.mat, col=terrain.colors(20))
contour(normal.mat, add=TRUE)

###'
###' #### Drawing a curve
###' To draw a curve of a function, use curve()
rm(x)
# x
curve(x^3) # Default is to plot between 0 and 1. Note: x here is a symbol

curve(x^3, from=-3, to=3, lwd=3, col="red") # More plotting options

###' Adding a curve to an existing plot
###' To add a curve to an existing plot, use curve() with add=TRUE

###' # Note: the x argument here and the x vector we defined above are different!
###' # Reminder: x here is a symbol

###'
###' #### You can plot anything!
###' The field of visualization has been filled with nerds,geeks, and hackers, because .... you can plot anything.
###' You can edit images as you like (e.g., ggimage package), make MEME (e.g., meme package), or create animation (e.g., animation).
###' We will not go that far, but there are still plenty to play with. The following is an example.

x0 = (0:200-100)/100
y0 = sqrt(1-x0^2)
center.top.x=0
center.top.y=0
radius.top=2
radius.bottom=radius.top/4
center.bottom.y=0
center.bottom.x = center.top.x+radius.bottom*c(-3,-1,1,3)
center.eyes.x = c(-1,1)*radius.top/6+center.top.x
center.eyes.y = radius.top*5/6+center.top.y
center.mouth.x = center.top.x
center.mouth.y = radius.top*0.5+center.top.y
radius.eyes = radius.top*0.1
radius.mouth = radius.top*0.2

plot(center.top.x,center.top.y,type='n',xlim = c(-radius.top,radius.top)+center.top.x,ylim = center.top.y+c(0,radius.top),xlab = '',ylab = '')
lines(center.top.x+x0*radius.top,center.top.y+y0*radius.top)
lines(center.bottom.x[1]+x0*radius.bottom,center.bottom.y+y0*radius.bottom)
lines(center.bottom.x[2]+x0*radius.bottom,center.bottom.y+y0*radius.bottom)
lines(center.bottom.x[3]+x0*radius.bottom,center.bottom.y+y0*radius.bottom)
lines(center.bottom.x[4]+x0*radius.bottom,center.bottom.y+y0*radius.bottom)
lines(center.eyes.x[1]+x0*radius.eyes,center.eyes.y+y0*radius.eyes)
lines(center.eyes.x[1]+x0*radius.eyes,center.eyes.y-y0*radius.eyes)
lines(center.eyes.x[2]+x0*radius.eyes,center.eyes.y+y0*radius.eyes)
lines(center.eyes.x[2]+x0*radius.eyes,center.eyes.y-y0*radius.eyes)
lines(center.mouth.x+x0*radius.mouth,center.mouth.y+y0*radius.mouth)
lines(center.mouth.x+x0*radius.mouth,center.mouth.y-y0*radius.mouth)
points(center.eyes.x,rep(center.eyes.y,2),pch='*',cex=5,col='brown')



###'
###' ## Statistical Modeling in R

###'
###' #### House Price data
###'
###'This dataset is from Kaggle \url{https://www.kaggle.com/tanyachawla412/house-prices}
###' The measured variables:
###'
###'  * Avg..Area.Income: Numerical data about the average area of the income where the house is located.
###'  * House.Age: Age of the house in years.
###'  * Number.of.Rooms:
###'  * Number.of.Bedrooms:
###'  * Area.Population:  Population of the area where the house is located.
###'  * Price:
###'  * Address: The only textual data in the dataset consisting of the address of the house.

setwd(dir.data)
house.df = read.csv('House_price.csv')
dim(house.df)
head(house.df)
###' At this moment, we cannot use the address directly in the model, let us remove it for now for simplicity
house.df$Address = NULL
head(house.df)

###'Some example questions we might be interested in:
###'
###' * What is the relationship between Avg..Area.Population and Price?
###' * What is the relationship between House.Age and Price?
###' * Can we predict Price from the other variables, and how good are the predictions?

###'
###' #### Exploratory data analysis
###' When you are given a dataset, it is generally a good practice to look at your data by visualization, summary statistics and tables.
###' When done in a structured way, this is called exploratory data analysis.
###'  E.g., you might investigate:
###'
###'  * What are the distributions of the variables?
###'  * Are there distinct subgroups of samples?
###'  * Are there any noticeable outliers?
###'  * Are there interesting relationship/trends to model?

###'
###'  Distributions of house price variables.
###'
###'  Please also note how the titles are generated with paste()
colnames(house.df) # These are the variables
par(mfrow=c(2,3), mar=c(4,4,2,0.5)) # Setup grid, margins
for (j in 1:ncol(house.df)) {
  hist(house.df[,j], xlab=colnames(house.df)[j],
       main=paste("Histogram of", colnames(house.df)[j]),
       col="lightblue", breaks=20)
}

###' What did we learn?
###'
###' We learned the shapes of the distributions of these variables. If the response variable is skewed, we may need to transform it. What we have looks fine.
###'
###' We also found that the number of bedrooms are not integers, but clustered around the integers.
###' We floor them for simplicity.
###' Why floor() instead of round()? There are values say, 2.50, but no values between 2.50 and 3.00. So it is more natural to convert 2.50 with 2 instead of 3.
###'
###' I am not sure why the number of rooms is not an integer neither, maybe from statements such as 2.75 bathrooms. I choose to leave it alone.
house.df$Number.of.Bedrooms = floor(house.df$Number.of.Bedrooms)

###'Next, we look at the correlations between these variables
house.cor = cor(house.df)
round(house.cor,3)

###'We did not see any strong correlations, but some moderate ones. Let us find the relatively bigger correlations, and the variable pairs that they represent:

house.cor[lower.tri(house.cor,diag=TRUE)] = 0 # Why only upper tri part?
###' find the indexes of the correlations larger than 0.2 in magnitude.  You may change it to higher value if they are more larger correlations.
###'
###'  arr.ind=T tell R that we need their row/column IDs in the matrix, instead of their position in the underlying vector.
arry.ind.big.cor = which(abs(house.cor)>0.2,arr.ind = T)
###' report the larger correlations and the variable pairs (row/coumn names) as a data.frame.
big.cor.w.vars = data.frame(cor=house.cor[arry.ind.big.cor],row=rownames(house.cor)[arry.ind.big.cor[,'row']],col=colnames(house.cor)[arry.ind.big.cor[,'col']])
#  big.cor.w.vars
big.cor.w.vars[order(abs(big.cor.w.vars$cor),decreasing = T),]

###'The biggest correlation is between the area income and the house price. This is of our interests, and we need to include the average area income into the model that predict house price.
###'
###'The second biggest correlation is between number of rooms and number of bedrooms. This is not of interests because it is not relevant to our target response (Price), and it is trivial that the number of bedrooms directly contribute to the number of rooms.

###'Visualizing relationships among variables, with pairs()
###'
###'Can easily look at multiple scatter plots at once, using the pairs() function.
pairs(house.df)

###' pairs(~ Price + Avg..Area.Income + House.Age +  Area.Population+  Number.of.Rooms , data=house.df) # If only need to plot for a subset of variables, can put a formula as the first argument.
###'
###' What a mess! When there are a lot of data points, it is difficult to tell where the bulk of the points lay because a pixel is filled as long as one point is there.
###'
###' An alternative is to present the density of the point in gray scale using hexbin plot. You need to install the package first.
library(hexbin)
hexplom(~ house.df, data=house.df)

###'
###' #### Inspecting relationships over a subset of the observations
###'
###'We know that the potential buyers of the houses with different sizes are different. A single person or couples with no kids are more likely to buy houses with less bedrooms, and family with more kids are more likely to buy larger houses.
###' It may be worth it to zoom in to a subset of the data and investigate the patterns, say, the 2Br houses.

house.df.2br = house.df[house.df$Number.of.Bedrooms ==2,]
house.df.2br$Number.of.Bedrooms=NULL
nrow(house.df.2br)
hexplom(~ house.df.2br, data=house.df.2br)

###'
###' #### Testing means between two different groups
###'Let see how the prices vary between houses with different numbers of bedrooms.

house.df$Number.of.Bedrooms = factor(house.df$Number.of.Bedrooms)
#par(mfrow=c(1,1))
boxplot(Price~Number.of.Bedrooms,data = house.df,  main="Price versus #Bedrooms",
        xlab="#Bedrooms", ylab="Price")

###'Visually, the prices slightly differ between 2Br and 3Br houses, and do not differ between 5Br and 6Br houses.
###'Now let us try simple two-sample t-tests:
table(house.df$Number.of.Bedrooms)
t.test(house.df$Price[house.df$Number.of.Bedrooms==2],house.df$Price[house.df$Number.of.Bedrooms==3])
t.test(house.df$Price[house.df$Number.of.Bedrooms==5],house.df$Price[house.df$Number.of.Bedrooms==6])

###'Confirms what we saw visually

###'
###' #### Linear regression modeling
###'
###'The linear model is arguably the most widely used statistical model, has a place in nearly every application domain of statistics
###'
###'Given response Y and predictors X1,...,Xp, in a linear regression model, we posit:
###'
###'  $Y=\beta_0+\beta_1 X_1+\ldots +\beta_p X_p+\epsilon$,where $\epsilon\sim N(0,\sigma^2)$
###'
###' Goal is to estimate parameters, also called coefficients $\beta_0$,$\beta_1$,..,$\beta_p$.
###'
###'Fitting a linear regression model with lm()
###'
###'We can use lm() to fit a linear regression model. The first argument is a formula, of the form Y ~ X1 + X2 + ... + Xp, where Y is the response and X1, ???, Xp are the predictors. These refer to column names of variables in a data frame, that we pass in through the data argument
###'
###'E.g., for the house data, to regress the response variable Price onto the predictors variables  Avg..Area.Income, House.Age,  Area.Population,  Number.of.Rooms  :

house.lm = lm(Price ~  Avg..Area.Income + House.Age +  Area.Population+  Number.of.Rooms , data=house.df)
class(house.lm) # A class or R object is usually a specialized "list"
names(house.lm) # Here are its components
house.lm # It has a special way of printing

###'
###' #### Utility functions
###'
###'Linear models in R come with a bunch of utility functions, such as coef(), fitted(), residuals(), summary(), plot(), predict(), for retrieving coefficients, fitted values, residuals, producing summaries, producing diagnostic plots, making predictions, respectively
###'
###'These tasks can also be done manually, by extracting at the components of the returned object from lm(), and manipulating them appropriately. But this is discouraged, because:
###'  The manual strategy is more tedious and error prone
###'
###'Once you master the utility functions, you will be able to retrieve coefficients, fitted values, make predictions, etc., in the same way for model objects returned by glm(), gam(), and many others
###'Retrieving estimated coefficients with coef()
###'So, what were the regression coefficients that we estimated? Use the coef() function, to retrieve them:

house.coef = coef(house.lm) # Vector of 3 coefficients
house.coef

###'What does a linear regression coefficient mean, i.e., how do you interpret it?

###'Displaying an overview with summary()
###'The function summary() gives us a nice summary of the linear model we fit:

summary(house.lm) # Special way of summarizing
###'This tells us:
###'
###'  The quantiles of the residuals: ideally, this is a perfect normal distribution
###'
###'The coefficient estimates
###'
###'Their standard errors
###'
###'P-values for their individual significances
###'
###'(Adjusted) R-squared value: how much variability is explained by the model?
###'
###'  F-statistic for the significance of the overall model
###'
###'Running diagnostics with plot()
###'
###'We can use the plot() function to run a series of diagnostic tests for our regression:


par(mfrow=c(2,2),mar=c(4,4,2,0.5))
plot(house.lm) # Special way of plotting
###'The results are pretty good: why?

###'  Residuals vs Fitted plot: points appear randomly scattered, no particular pattern
###'
###'Normal Q-Q plot: points are mostly along the 45 degree line, so residuals look close to a normal distribution
###'
###'Scale-Location and Residuals vs Leverage plots: no obvious groups, no points are too far from the center
###'
###'There is a science (and an art?) to interpreting these; you will learn a lot more in the regression course, and the other more advanced statistical courses.
###'
###' There are six types of plots that you can choose from using plot(lm).
###' If you need other types of plots, you need to generate them manually. E.g., fitted vs actual.
###'
###'Retrieving fitted values with fitted()
###'What does our model predict for the price, and how do these compare to the actual? Use the fitted() function, then plot the actual values versus the fitted ones:
dev.off()
house.fits = fitted(house.lm)
plot(house.fits, house.df$Price, main="Actual versus fitted values",
     xlab="Fitted values", ylab="Actual Prices")
abline(a=0, b=1, lty=2, col="red")


###'Making predictions with predict()
###'
###'Suppose you are considering buying a four year old house with 7 or 15 rooms in Durham. For Durham, the average income is 71190, and the population is 14638.
###'What does your linear model tells you to pay?
###'What problem does your prediction have?

house.new = data.frame(Avg..Area.Income=rep(71190,2),Area.Population=rep(14638,2),Number.of.Rooms=c(7,15),House.Age=rep(4,2)) # Must set up a new data frame
house.pred = predict(house.lm, newdata=house.new) # Now call predict with new df
house.pred


###'Here are some handy shortcuts, for fitting linear models with lm() (there are also many others):
###'  No intercept (no $\beta_0$ in the mathematical model): use 0 + on the right-hand side of the formula, as in:
summary(lm(Price ~ 0 + Avg..Area.Income + House.Age +  Area.Population+  Number.of.Rooms, data=house.df))
###'Include all predictors: use . on the right-hand side of the formula, as in:
summary(lm(Price ~ ., data=house.df))
###'Include interaction terms: use : between two predictors of interest, to include the interaction between them as a predictor, as in:

summary(lm(Price ~ Avg..Area.Income + House.Age +  Area.Population+  Number.of.Rooms+ House.Age:Area.Population, data=house.df))


###'
###' #### Practice
###' 1. Read the car medical charge dataset from https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv
###' Let us call it insurance.df.
###' You can read https://www.kaggle.com/mirichoi0218/insurance to learn more about the dataset. The goal is to predict the medical cost billed by the health insurance.
###' Check the data type of each column of insurance.df
###'
###' 2. To investigate the marginal distribution of each variable, plot the histogram for each numeric variables,
###' and barplot for each categorical variable in the datset. For the latter, you will need R functions barplot() and table()
###' Will you recommend any transformation of any of the numeric variables.
###'
###' 3. To investigate the association between the medical charges and each of the other variables, draw a scatter plot using "charges" at y-axis, and each of the other numeric variable at x axis,
###' and a side-by-side box plot of "charges" using each of the categorical variable as the grouping variable.
###'
###' 4. You can also calculate the marginal correlation of the charges and each of the other numeric variables.
###' What will happen to the correlations if you take the log of the charges?
###'
###' 5. Test whether there is any difference in the average of the charges between smokers and non-smokers
###'
###' 6. Compute, using `lm()`, a linear regression model of the log of `charges`  on the other variables. Save the results as `insurance.lm`. Using `coef()`, display the coefficients (intercept and slope).
###'
###' 7. Plot the diagnostic plots of insurance.lm. Do you think the fit is good?
###'
###' 8. Regardless how good you think the model fit is, predict the medical cost at the original scale for a 28 years-old male smoker with bmi=30, and no children living in southeast region.


insurance.df = read.csv('https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv')
dim(insurance.df)
head(insurance.df)
table(sapply(insurance.df,typeof))

par(mfrow=c(2,2),mar=c(4,4,2,0.5))
for(kk in names(insurance.df)[sapply(insurance.df,typeof)!='character']) hist(insurance.df[[kk]],main = paste('Histogram of ',kk),xlab = kk)

par(mfrow=c(1,3),mar=c(4,4,2,0.5))
for(kk in names(insurance.df)[sapply(insurance.df,typeof)=='character']) barplot(table(insurance.df[[kk]]),main = paste('Barplot of ',kk),xlab = kk)

barplot(table(insurance.df$region))





cor(insurance.df$charges,insurance.df[sapply(insurance.df,typeof)!='character'][-4])

cor(log(insurance.df$charges),insurance.df[sapply(insurance.df,typeof)!='character'][-4])

par(mfrow=c(1,3),mar=c(4,4,2,0.5))
for(kk in names(insurance.df)[sapply(insurance.df,is.numeric)][-4]) {
  plot(insurance.df[[kk]],insurance.df$charges,xlab=kk,ylab='charges')
  lines(lowess(insurance.df[[kk]],insurance.df$charges))
}

par(mfrow=c(1,3),mar=c(4,4,2,0.5))
for(kk in names(insurance.df)[!sapply(insurance.df,is.numeric)]) boxplot(insurance.df$charges~insurance.df[[kk]],xlab=kk,ylab='charges')


t.test(insurance.df$charges[insurance.df$smoker=='yes'],insurance.df$charges[insurance.df$smoker=='no'])

insurance.lm = lm(log(charges)~.,data=insurance.df)
coef(insurance.lm)
summary(insurance.lm)

par(mfrow=c(2,2),mar=c(4,4,2,0.5))
plot(insurance.lm,col=factor(insurance.df$smoker))
plot(insurance.lm,col=factor(insurance.df$sex))
plot(insurance.lm,col=factor(insurance.df$region))
plot(insurance.lm,col=factor(insurance.df$children))

par(mfrow=c(2,2),mar=c(4,4,2,0.5))
plot(insurance.df$age,insurance.df$charges,col=as.factor(insurance.df$sex),pch=as.integer(as.factor(insurance.df$sex)))
legend(x='topleft',legend = levels(as.factor(insurance.df$sex)),col = 1:length(levels(as.factor(insurance.df$sex))),pch = 1:length(levels(as.factor(insurance.df$sex))))
plot(insurance.df$age,insurance.df$charges,col=as.factor(insurance.df$smoker),pch=as.integer(as.factor(insurance.df$smoker)))
legend(x='topleft',legend = levels(as.factor(insurance.df$smoker)),col = 1:length(levels(as.factor(insurance.df$smoker))),pch = 1:length(levels(as.factor(insurance.df$smoker))))
plot(insurance.df$age,insurance.df$charges,col=as.factor(insurance.df$region),pch=as.integer(as.factor(insurance.df$region)))
legend(x='topleft',legend = levels(as.factor(insurance.df$region)),col = 1:length(levels(as.factor(insurance.df$region))),pch = 1:length(levels(as.factor(insurance.df$region))))
plot(insurance.df$age,insurance.df$charges,col=as.factor(insurance.df$children),pch=as.integer(as.factor(insurance.df$children)))
legend(x='topleft',legend = levels(as.factor(insurance.df$children)),col = 1:length(levels(as.factor(insurance.df$children))),pch = 1:length(levels(as.factor(insurance.df$children))))


customer.new = data.frame(age=28,sex='male',bmi=30,children=0,smoker='yes',region='southeast')
exp(predict(insurance.lm,newdata = customer.new))


