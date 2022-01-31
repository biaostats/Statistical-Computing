#' ---
#' title: "R Basics Lab"
#' author: Biao Zhang
#' date: "September 10, 2021"
#' ---
#'

###'
###' Text manipulation
###'
###' 1. (1 points for each of a-c, 2 points for each of d-g) mtcars is a data frame that was extracted from the 1974 Motor Trend US magazine,
###' and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973-74 models).
###' The names of the cars are in the rownames(mtcars). Define car.names = rownames(mtcars)
###' Answer each of the question using no more than three lines of code.
###' 
###' a). Count the number of characters in each car name, excluding the space.
###' b). Extract the rows for the cars that start with letter D.
###' c). Functions toupper and tolower convert the lower/upper cases letters to upper/lower cases. Try them on car.names
###' d). Calculate the frequency of the characters in the car names. You may need R functions, strsplit, table, unlist
###' e). Find the Toyota car with the largest hp without using numeric indexing or typing any of the rownames.
###' f). Find the average mpg of all Merc cars.
###' g). Find the average weight (wt) of all cars with 8 cylinders (cyl)
###'
###' Solutions:
car.names = rownames(mtcars)
###' a)
nchar(car.names)
###' b)
mtcars[grepl('D', substr(car.names,1,1), fixed = T),]
###' c)
toupper(car.names)
tolower(car.names)
###' d) For letters, if it is not case sensitive,
table(unlist(strsplit(toupper(car.names), split = '')))
###' If it is case sensitive.
table(unlist(strsplit(car.names, split = '')))
###' e) 
max(mtcars[grepl('toyota', car.names, ignore.case = T),"hp"])
###' f)
mean(mtcars[grepl('merc', car.names, ignore.case = T),"mpg"])
###' g)
mean(mtcars[mtcars$cyl == 8,"wt"])
###' 
###' 
###' 2. Consider the state.df data frame. (1 points for each of a, 2 points for each of b-d)
###'
###' (a) Extract the rows whose division contain "Central"
###' (b) Calculate the average life expancy (Life.Exp) for the states whose names contains more than one word
###' (c) Are all the states in a division with key word "South" also has value "South" for the variable Region?
###' (d) Find the median murder rates for the states with no more than 60 Frost days per year.
###' 
###' Solutions:
state.df = data.frame(state.x77, Region=state.region, Division=state.division)
###' a)
state.df[grepl('central', state.df$Division, ignore.case = T),]
###' b)
mean(state.df[grepl(' ', rownames(state.df), fixed = T), "Life.Exp"])
###' c)
all(state.df[grepl('south', state.df$Division, ignore.case = T),"Region"] == "South") 
###' d)
median(state.df[state.df$Frost <= 60, "Murder"])
###'
###'      
###' 3. Consider the downloaded folder dataexample, and state.df together (3 points)
###'
###' (a) Display the names of the pdf file for the states in the south region.
###' (Hints: there is no New Hampshire in dataexample, and all the pdf files and all state names can be ordered alphabetically.)
###' 
###' Solution:
setwd("H:\\MATH796\\Week 2\\dataexample")
filenames = list.files(getwd())
pdfnames = filenames[grepl('.pdf', filenames)]
south.region = gsub(' ', '-', rownames(state.df[grepl('South', state.df$Region),]), fixed = T)
sapply(paste0("^",south.region), grep, x = pdfnames, ignore.case = T, value = T, USE.NAMES = F)


