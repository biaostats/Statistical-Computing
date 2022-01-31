#' ---
#' title: "R Data files, plot and modeling"
#' author: "Qi Zhang (including materials from Dr. Ryan Tibshirani's class at CMU)"
#' date: "August 01, 2021"
#' ---

###' 
###'## Reading in and reordering data
###' Previously we have used the datasets built in R.
###' Next we will read data from the outside.
###' 
###' Depending on the original format, there are many functions for reading data into R, including, but not limited to the following:
###' 
###' * readLines(): reading in lines of text from a file or webpage; returns vector of strings
###' * read.table(): read in a data table from a file or webpage; returns data frame
###' * read.csv(): like the above, but assumes comma separated data; returns data frame
###' For now we will focus on read.table(), read.csv() and their counterparts write.table(), write.csv(), respectively

###' 
###' #### Reading in data from a previous R session
###' Generally there are two ways to read/write in data in specialized R formats:
###'  readRDS(), saveRDS(): functions for reading/writing single R objects from/to a file
###'load(), save(): functions for reading/writing any number of R objects from/to a file

###'
###'#### read.table(), read.csv()
###'Then read.table() reads a data matrix into R. It works as in:
###'
###'  read.table(file=file.name, sep=" "), to read data from a local file on your computer called file.name, assuming (say) space separated data
###'  
###'read.table(file=webpage.link, sep="\t"), to read data from a webpage up at webpage.link, assuming (say) tab separated data
###'
###'The function read.csv() is just a shortcut for using read.table() with sep=",". (But note: these two actually differ on some of the default inputs!)
###'
###' If you are using RStudio, you can also read data by clicking "Import Dataset" in the top right panel. This interface produce a chunk of R code that read the data, 
###' you can also change the default options if you are not happy with what R studio proposes. 
###'
###' The examples used here are all directly from third-party data repository. Thus they are less "clean" then what you may normally see in class demos, and some data processing is needed. 
###' 
###' I choose to do so to emphasize the fact that data cleaning is part of statistician's data routine. Even if when we are given a "clean dataset", we cannot take it for granted.
###' 
###' In the examples, I will also expose you to some data cleaning techniques that may be useful. For now, you are not required to learn these particular techniques. 
###' But you need to start practicing data cleaning whenever you are given a dataset.   
###' 
###' This data table is comma separated, so we can use read.csv()

madmen.df =   read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/mad-men/show-data.csv")
head(madmen.df)
sapply(madmen.df, class)
table(madmen.df$Score.Y)
###' Intuitively, Score.Y are supposed to be numeric. See some weird entries, let us find out why by looking at these rows.
madmen.df[which(madmen.df$Score.Y=="#DIV/0!"),]
###' Score.Y = Score/Years.Since. These weird entries are from 0 as the denominator, and Years.Since is 0 if the show just ended.
###' 
###' Score is the total score of a performer's career after the hit show ends, and Score.Y is a normalized version by years after the show.
###' 
###' I feel like it is reasonable to convert 0's in Years.Since to 1, and it is also equivalent to use Score to approximate Score.Y for these performers. 
###' You may have different opinions on how it should be handled, which is fine. Just make sure to document your reasoning, and make both of the original and the processed data available.
madmen.df$Score.Y = as.numeric(madmen.df$Score.Y)
madmen.df$Score.Y[is.na(madmen.df$Score.Y)] = madmen.df$Score[is.na(madmen.df$Score.Y)]
summary(madmen.df$Score.Y)
dir.data = "D:\\Dropbox\\courses\\stat_computing\\data\\" 
setwd(dir.data)

###'Helpful input arguments when reading/writing data files
###'The following inputs apply to either read.table() or read.csv() (though these two functions actually have different default inputs in general
###'
###' * header: boolean, TRUE is the first line should be interpreted as column names
###' * sep: string, specifies what separates the entries; empty string "" is interpreted to mean any white space
###' * quote: string, specifies what set of characters signify the beginning and end of quotes; empty string "" disables quotes altogether
###' * Other helpful inputs: skip, row.names, col.names. You can read about them in the help file for read.table()
###' * write.table(), write.csv()
###'To write a data frame (or matrix) to a text file, use write.table() or write.csv(). These are the counterparts to read.table() and read.csv()
write.table(madmen.df, file="show-data.txt",sep='\t',row.names=F,quote = T)

###' This data table is tab separated, so use read.table, and specify sep="\t"
madmen.df.tab = read.table("show-data.txt",header=T,sep='\t',quote = "\"")
head(madmen.df.tab)
sapply(madmen.df.tab, class)
###' You can also save it as csv
write.csv(madmen.df, file="show-data_noNA.csv",row.names=F,quote = T)
madmen.df2 = read.csv("show-data_noNA.csv",header=T,quote = "\"")


###' Try what will happen if you do the following
write.csv(madmen.df, file="show-data_no_quote.csv",row.names=F,quote = F)
try( madmen.df2 <- read.csv("show-data_no_quote.csv",header=T,quote = "\""))

###' What happened? It turns out that some of the character strings contain comma. 
###' 
###' If the quotation marks are not written around these strings in the .csv file, read.csv will treat one such string as more than one entry.
###' The consequence is that some rows will have more elements than the others, and the result will not be a matrix, which triggers the error. 
###' 

###' 
###' #### Reordering data
###'Sometimes it is convenient to reorder our data, say the rows of our data frame (or matrix). 
###'
###'Recall:
###'  The function order() takes in a vector, and returns the vector of indices that put this vector in increasing order
###'Set the input decreasing=TRUE in order() to get decreasing order
###'
###'We can apply compute an appropriate vector of indices, and then use this on rows of our data frame to reorder all of the columns simultaneously
###'
###'Examples of reordering:
###' Suppose we wanted to rank the data based on their score per year in descending order.
i.score.y = order(madmen.df$Score.Y, decreasing=TRUE) # Order by decreasing score
madmen.df.score.y = madmen.df[i.score.y,] # Reorder rows by decreasing socre.
head(madmen.df.score.y)
###' Suppose we wanted to reorder the rows by show name, alphabetically
i.show = order(madmen.df$Show) # By show name
madmen.df.show = madmen.df[i.show,] # Reorder rows by show name
head(madmen.df.show)

###' You can save not just data frame, but any objects to an R data file using save()
###' The following two save the same two objects
save(i.show,i.score.y,file = 'i1.rdata')
save(list=c('i.show','i.score.y'),file='i2.rdata')
###' The following save all objects in the global environment, why?
###' 
save(list=ls(),file='all.rdata')
###' Using load() to load the saved objects to the global environment.  
rm(list=c('i.show','i.score.y'))
ls()
load('i1.rdata')
ls()


###' 
###' #### Practice
###' 1.Read https://github.com/nytimes/covid-19-data/blob/master/colleges, 
###' read the dataset https://raw.githubusercontent.com/nytimes/covid-19-data/master/colleges/colleges.csv into R, and save it as an object covid.college
covid.college = read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/colleges/colleges.csv")
###' 
###' 2. Reorder the rows in the descending order the the total cases, and call it covid.college.by.case.
i.case.college = order(covid.college$cases, decreasing = T)
covid.college.by.case = covid.college[i.case.college,]
###' 
###' 3. Save the reordered covid.college.by.case as a txt file separated by tab "\t" with the name covid_college_by_case.txt in the folder where you save the materials for this course. 
setwd("H:/MATH796/Week 3")
write.table(covid.college.by.case, file = "covid_college_by_case.txt", sep = '\t', row.names = F, quote = T)
###' 
###' 4. Read the txt file you have just created, and call it covid.college.my. If you get an error here, it may be because you have missed something when creating the txt file.
covid.college.my = read.table("covid_college_by_case.txt", header = T, sep = '\t', quote = "\"")
###' 

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
###' e.g., if this column is called "key", and X$key[2] == Y$key[5], then the 2nd row of X and the 5th row of Y represent the same unit(subject).
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
###' We may be also able to create NYC level census data that match NCY covid data by adding the county level data within NYC border.
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
covid0603.county$region_id <- id.region.covid
census2018.pe$region_id <- id.region.census

###' merge() function merge two data frames, and match the rows by the argument "by".
###' If you set all=F, the units unique to each data frame will be removed. Otherwise, (some of) them will be kept.
dt.covid.census <- merge(x=covid0603.county,y=census2018.pe,by='region_id',all=F)
dim(dt.covid.census)

###'The merge() function tries to merge two data frames according to common columns, as in: merge(x, y, by.x='SomeXCol', by.y='SomeYCol'), to join two data frames x, y, by matching the columns SomeXCol and SomeYCol
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
ne.states = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rohde Island", "Vermont")
i.covid0603.ne = sapply(ne.states, grepl, x = covid0603.county$state, ignore.case = T)
i.covid.college.ne = sapply(ne.states, grepl, x = covid.college$state, ignore.case = T) 
covid0603.ne = covid0603.county[as.logical(rowSums(i.covid0603.ne)),]
covid.college.ne = covid.college[as.logical(rowSums(i.covid.college.ne)),]
dim(covid0603.ne)
dim(covid.college.ne)
###'  
###' 2. Then, please merge covid.college.ne and covid0603.ne. We want to keep every row in covid.college.ne. But we are not interested in the counties with no college. 
###' Let us call the final output covid.college.w.county.ne. What is the dimension of this data frame? Can you guess how merge function handles it when one row of Y can be mapped to multiple rows of X?
id.college.ne = paste(covid.college.ne$county, covid.college.ne$state, sep = '_')
###' Analysis
length(intersect(id.college.ne, covid0603.ne$region_id))
id.college.ne.only = sort(setdiff(id.college.ne, covid0603.ne$region_id))
length(id.college.ne.only)
id.covid.ne.only = sort(setdiff(covid0603.ne$region_id, id.college.ne))
length(id.covid.ne.only)
id.college.ne.only
id.covid.ne.only
###' Correct the mistake
id.college.ne = gsub(' ', '', id.college.ne)
covid.college.ne$region_id = id.college.ne
###' Final Answer
covid.college.w.county.ne = merge(covid.college.ne, covid0603.ne, by = 'region_id', all.x = T)
dim(covid.college.w.county.ne)
