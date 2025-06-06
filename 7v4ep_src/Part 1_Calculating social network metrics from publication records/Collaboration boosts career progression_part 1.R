### Project: "Collaboration enhances career progression in academic science, especially for female researchers"
## Authors: "Jessica E.M. van der Wal*, Rose Thorogood, and Nicholas P.C. Horrocks"
## Date: 20 July 2021
## *e-mail: jessicavanderwal1@gmail.com

### PART 1: CALCULATING SOCIAL NETWORK METRICS FROM PUBLICATION RECORDS

# Input: any given list of academic authors with associated Scopus Author Identifiers (SAI)
# Function: access the publications available on Scopus, data wrangling, calculation of social network metrics.

### Methods: 
#(1) Dredge publications using AuthorID
#(2) Combine authorIDs of one author
#(3) Turn into publication*author matrix
#(4) Get association matrix (ID*ID), egonet metrics and visualise social networks
#(5) Get weighted edgelist, needed to get last egonet metric: strength
#(6) Retrieve publication years (first and last), and publications per year
#(7) Merging dataframes and applying restrictions 
#(8) Calculating career gaps
#(9) Calculating social network metrics for first 10 ten years

############################################################################################
###			(1) Dredge publications using AuthorID
############################################################################################
library(readxl) 
library(readr)
#install.packages("http://cran.r-project.org/src/contrib/Archive/bibliometrix/bibliometrix_2.3.2.tar.gz", repos=NULL, type="source")
library(bibliometrix) 
library(rscopus) 
library(dplyr)
library(stringr)
library(readxl)

## Load your datafile with author names and associated Scopus IDs. Label the columns: 'Firstname', 'Surname' and 'AU_ID'
# NOTE: Create a seperate row for each Scopus ID (so if an author has two Scopus IDs, create two seperate rows)
# NOTE: Make sure you (manually) add an identifier to authors that have same first letter + surname, so that they are recognised as separate authors.
# For purposes of confidentiality, author names and Scopus Author Identifiers (SAI) are not be made available."
# We therefore present a hypothetical dataset of the authors' SAIs, so that the code 'Social authors code_part1.Rmd' can still be run.
df = read_csv("authors_input_demo.csv") 

## Modify author names to remove accents, make all uppercase and format to: first letter of first name + surname 
names = as.data.frame(paste(df$Firstname, df$Surname,sep = ".")) #join first and last name with a full stop
names <- apply(names,2,toupper)#change all letters of name into uppercase
names=as.data.frame(names) 
names[,1] = as.character(names[,1]) #make names characters
for(i in names(names)){ #loop to remove accents from names, take first initial 
  names[[i]]= paste(sapply(strsplit(sub('^(\\w+).*\\b(\\w+)$', '\\1 \\2', as.character(names[[i]])), " "), function(r) paste0(substr(r[1], 1, 1), ".", substr(r[2], 1, 50)))) 
  names[[i]] = iconv(names[[i]],from="UTF-8",to = "ASCII//TRANSLIT") 
}
df["author.name"] = names
df$author.name = as.factor(df$author.name)
df = df[order(df$author.name),] #order by author name

### Extract publication details using Scopus ID

## Create vector for dredge
authors = df$AU_ID		
# Get API key from https://dev.elsevier.com/apikey/create- Need to change this depending on IP address! 
# NOTE: there is a limit to the number of times per week that you can use the API for Scopus searches!! If it stops working, this may be why - you may request multiple API keys (linked to email address).
apik = "09838b4021c9643603a665353ac31835"	#this is an expired example

res = retrievalByAuthorID(authors, apik)	
M = res$M	# object of biblio data, each row is publication. 
P = res$authorDocuments # to get a list containing a bibliographic data frame for each author.
# See for more info: https://github.com/massimoaria/bibliometrix/blob/master/R/retrievalByAuthorID.R and http://bibliometrix.org/documents/bibliometrix_Report.html

#set working directory where you want to save the files generated in the next step to save results in right folder
setwd("~/output") 

## Create empty dataframe 
M <- data.frame(matrix(ncol = 32, nrow = 0))
colnames(M) = c("AU_ID","AU","C1_ID","C1","nAU","nC1","TC","SO","DT","TI","PII","DI","EID","PY","CDD","URL","UT","AU1","ISSN","EISSN","PAG","AB","PT","SUBTYPE","DE","SO_ID","CR","DB","ID","AU_CO","AU1_CO","RP") #These are the column names of the data

## Then write loop to go through all Scopus IDs, adding the output to the first dataframe
# NOTE: create 'output' folder in your working directory before running the following code
for (i in 1:length(authors)){
  tryCatch({ #using tryCatch () to go around error https://stackoverflow.com/questions/14748557/skipping-error-in-for-loop. So here it skips the authorIDs with an error and keeps going
    res = retrievalByAuthorID(authors[i], apik)
    M2 = res$M
    output <- rbind(M,M2)
    write.csv(output, paste0(output$AU_ID[1],".csv"), row.names=F) #creates seperate csv's for each Scopus ID
  }, error=function(e){})
}

# NOTE: if you get the following (or similar) error: "'names' attribute [26] must be the same length as the vector [15]":
# That means that Scopus does not recognise that ID
# Problem seems to be that API sometimes uses different IDs than Scopus shows (and which we use..)
# Solution: extract the info of the IDs it does recognise, and extract the rest by hand (see 1.3)
# This is only a valid approach if API and Scopus show same results. Checked, and for 120 cases, results only differed for 2 IDs, with a difference of 1. 
# Editted the loop so that it creates seperate csv's for each Scopus ID, using truCatch () to go around error https://stackoverflow.com/questions/14748557/skipping-error-in-for-loop. So here it skips the Scopus IDs with an error and keeps going
# https://rdrr.io/github/dsidavis/RScopusAPI/src/R/scopus.R

### Dredge the remaining Scopus IDs for which API didn't work.

# Read in dataframe with the author names for which the above API dredging didn't work. It must have the following columns: 'AU_ID' and 'author.name'.

# For the purpose of demonstration, let's assume that the API dredging for all 1 of the demo authors did not work (Rose Thorogood)
authorswithAPInotworking = df[3,]

## Now go to Scopus, and manually download .bib files for all these authors (download ALL info available-tick all boxes). You can only download a max of 2000 articles at a time, so you have to do it in bouts of max 20 authors batches at a time.
Thorogood <-  scopus2df(readFiles('scopus_Thorogood.bib')) #example in which you import the papers of Rose Thorogood

# Add these into one dataframe
Scopus = rbind(Thorogood)#NB in this example only a single author so rbind is not doing anything

## Remove accents and turn letters into plain
Scopus$AU=iconv(Scopus$AU, from="UTF-8",to = "ASCII//TRANSLIT") 
Scopus$SO=iconv(Scopus$SO, from="UTF-8",to = "ASCII//TRANSLIT") 

## Split names into separate columns
x <- strsplit(as.character(Scopus$AU), ";")
l <- lengths(x)  
m <- max(l)
x <- t(sapply(x[as.logical(l)], function(a) c(a, rep("",m-length(a)))))
x = as.data.frame(x)
## Select first letter of first name + surname
for(i in names(x)){
  firstname = word(x[[i]],-1)
   initial = substring(firstname, 1, 1)
   surname = word(x[[i]],-2)
   surname[is.na(surname)] <- ""
   tot = as.data.frame(paste(initial, surname,sep = "."))
   tot[tot=="."]=""  
   x[[i]] <- tot  #join first and last name with a full stop
   }

#if you want to include whole last name (including ' van der'  etc):
# surname = sub(x[[i]], pattern = " [[:alpha:]]*$", replacement = "")
# surname = gsub(" ", "", surname, fixed = TRUE)


## Remove duplicated author names
x[t(apply(x, 1, duplicated))] <- NA 

## Remove NAs 
x[is.na(x)]=""

ncol.old = ncol(x)

# NOTE: make sure that the authors that occured in both the API dataframe and the hand-dredged one are spelled exactly the same, so that it is later recognised that the Scopus IDs belong to the same person (for example J.WAL and J.VANDERWAL)

## Replace authorname column in Scopus (surname+first name) by new column in which: first name+surname (to match API dataframes)
Scopus$AU = apply(x,1,function(x) paste(x[!is.na(x)],collapse=";"))

# Add a column per author, indicating which papers belong to them (0 or 1)
authors = as.character(unique(authorswithAPInotworking$author.name))
x[is.na(x)]=""
for(i in (authors)){  
  x$i = rowSums(x == i)
  colnames(x)[colnames(x) == 'i'] <- i 
}

ncol.new = ncol(x)

# Only select the columns added on in the previous step
subset = x[(ncol.old+1):ncol.new]

## Get number of authors per paper

subset$nAU = rowSums((x[1:ncol.new]!="")) 

## Now create seperate csv file per author name, in the same format at the API dataframes, saved in ~/scopus_output
# NOTE: create 'scopus_output' folder in your working directory before running the following code
for(j in colnames(subset[-2])){ #minus the last column, in this example the 2nd column, because 1 focal authors in total
  sel = select(subset,j,nAU)
  sub = cbind(sel,Scopus)
  sub = sub[sub[j]==1,]
  sub$author.name = rep(colnames(sub[1]),nrow(sub))
  sub$AU_ID = rep(NA, nrow(sub))
  sub$C1_ID	= rep(NA, nrow(sub))
  sub$nC1	= rep(NA, nrow(sub))
  sub$PII	= rep(NA, nrow(sub))
  sub$EID	= rep(NA, nrow(sub))
  sub$CDD	= rep(NA, nrow(sub))
  sub$URL	= rep(NA, nrow(sub))
  sub$UT	= rep(NA, nrow(sub))
  sub$AU1	= rep(NA, nrow(sub))
  sub$ISSN	= rep(NA, nrow(sub))
  sub$EISSN	= rep(NA, nrow(sub))
  sub$PAG	= rep(NA, nrow(sub))
  sub$PT	= rep(NA, nrow(sub))
  sub$SUBTYPE	= rep(NA, nrow(sub))
  sub$SO_ID	= rep(NA, nrow(sub))
  sub$AU_CO	= rep(NA, nrow(sub))
  sub$AU1_CO	= rep(NA, nrow(sub))
  sub = sub[,c("AU_ID","AU","C1_ID","C1", "nAU","nC1", "TC",	"SO",	"DT",	"TI",	"PII",	"DI",	"EID",	"PY",	"CDD",	"URL",	"UT",	"AU1",	"ISSN",	"EISSN",	"PAG",	"PT",
               "SUBTYPE",	"DE",	"SO_ID","DB",	"ID",	"AU_CO","AU1_CO",	"RP","author.name")]
  write.csv(sub, file.path("~/scopus_output",paste0(sub$author.name[1],"", ".csv")), row.names=FALSE) #Create file per Scopus ID
}

############################################################################################
###			(2) Combine authorIDs of one author
############################################################################################
library(readr) 
library(asnipe) 
library(sna) 
library(data.table) 
library(dplyr) 
library(doBy) 
library(plyr) 
library(gridExtra)

## Import all API output files first
setwd("~/output_demo")    

file_list <- list.files() 
## Create empty dataframe to give structure
tot <- data.frame(matrix(ncol = 30, nrow = 0))
colnames(tot) = c( "AU_ID","AU","C1_ID","C1","nAU","nC1","TC","SO","DT","TI","PII","DI","EID","PY","CDD","URL","UT","AU1","ISSN","EISSN","PAG","PT","SUBTYPE","DE","SO_ID","DB","ID","AU_CO","AU1_CO","RP")

## Remove accents and turn letters into plain
for (file in file_list){ 
  total <- read_csv(file) 
  total$AU=iconv(total$AU, to = "ASCII//TRANSLIT") 
  total$SO=iconv(total$SO, to = "ASCII//TRANSLIT") 
  tot=rbind(tot,total) 
}
tot=subset(tot, select=c(-AB, -CR))
tot = tot[!is.na(tot$AU_ID),]#take out NAs..dont understand why they are here..

## Select first letter of first name + surname
# First split of authorlist in seperate cells
x <- strsplit(as.character(tot$AU), ";")
l <- lengths(x)  ## R 3.3.0 onward
m <- max(l)
x <- t(sapply(x[as.logical(l)], function(a) c(a, rep("",m-length(a)))))
x = as.data.frame(x)
# Many names spelled slightly different. To reduce this error, we select first letter of first name + surname
for(i in names(x)){
  x[[i]]= paste(sapply(strsplit(sub('^(\\w+).*\\b(\\w+)$', '\\1 \\2', as.character(x[[i]])), " "), function(r) paste0(substr(r[1], 1, 1), ".", substr(r[2], 1, 50)))) 
}
x[x=="NA.NA"]=""

## Remove duplicated author names
x[t(apply(x, 1, duplicated))] <- NA 
## Paste back string of authors in right format
tot$AU = apply(x,1,function(x) paste(x[!is.na(x)],collapse=";"))

## Order by Scopus ID
tot = tot[order(tot$AU_ID),]

## Add author name to 'tot', using 'df'
author.details = unique(df[c("AU_ID", "author.name")]) #list of all Scopus IDs
author.details = author.details[order(author.details$AU_ID),] #order according to Scopus ID
tot.subset = subset(tot, select = c("AU_ID")) #only select Scopus ID
tot.subset = tot.subset[order(tot.subset$AU_ID),] #order according to Scopus ID
d = merge(author.details,tot.subset, by = "AU_ID") 
d = as.data.frame(d)
tot$author.name <- d$author.name
length(d[,1]) 

## Import the entries manually dredged from Scopus
setwd("~/scopus_output")    
file_list <- list.files()

## Create empty dataframe for structure
tot2 <- data.frame(matrix(ncol = 31, nrow = 0))
colnames(tot2) = c( "AU_ID","AU","C1_ID","C1","nAU","nC1","TC","SO","DT","TI","PII","DI","EID","PY","CDD","URL","UT","AU1","ISSN","EISSN","PAG","PT","SUBTYPE","DE","SO_ID","DB","ID","AU_CO","AU1_CO","RP", "author.name")

for (file in file_list){
  total <- read_csv(file) 
  tot2=rbind(tot2,total)
}
tot2=tot2[!duplicated(tot2),] #remove duplicated lines
tot3A=rbind(tot,tot2) #combine both dataframes, giving a dataframe with all papers from all authors. If this gives an error, check that all the columns are the same!
tot3A=tot3A[rowSums(is.na(tot3A)) != ncol(tot3A), ] #remove any rows with only NAs
tot3A$SO = as.factor(tot3A$SO)

#remove duplicates per author name, by looking at both the author name and the publication title
tot3A=tot3A[!duplicated(tot3A[,c("author.name", "TI")]),]

#in case there are duplicates
#tot3A = distinct(tot3A[,c("author.name", "TI")],)

## Look at distribution of number of authors per paper
#Fig ESM3C

ggplot(tot3A, aes(x = nAU)) + geom_histogram(binwidth = 0.25, colour = "black", fill = "white") + scale_x_log10()+  labs(tag = "C")+
  xlab("Number of authors/paper (log scale)") + ylab("Frequency") + theme_bw()+
  theme(plot.margin=unit(c(.5,.5,.2,.5),"cm"))+
  theme(# Axis.text.y = element_blank(), #remove y axis tick labels
    axis.text.y = element_text(size=12, colour="black"), # Change size of y axis labels
    axis.text.x = element_text(size=12,colour="black"), # Change size of x axis numbers
    axis.title.x = element_text(size=12,vjust=-0.9,colour="black"), # Change size and font of x axis title and move it down a bit
    panel.grid.major = element_blank(), # Formatting to create blank plot with box around it
    panel.grid.minor = element_blank(), # Formatting to create blank plot with box around it
    axis.line = element_line(colour="black"),
    panel.background = element_rect(colour = "black", size=1, fill=NA),
    legend.key = element_rect(fill = "white", color = NA), # Make background of symbols in legend white
    legend.text=element_text(size=12),
    legend.title = element_text(size=12)) +
  geom_vline(xintercept = 20, color = "royalblue", size=1.5)

#NOTE: manually check journal names (tot3A$SO) to check for spelling mismatches. Change manually, for example "tot3A$SO[tot3A$SO=="CURRENT BIOLOGY : CB"]="CURRENT BIOLOGY"

### Apply Restrictions

## Restriction: only ARTICLES, ARTICLES IN PRESS,LETTER, NOTE, REVIEW
tot3A$DT = as.factor(tot3A$DT)
table(tot3A$DT)
tot3B = tot3A[tot3A$DT!="BOOK"&tot3A$DT!="BOOK CHAPTER"&tot3A$DT!="CHAPTER"&tot3A$DT!="CONFERENCE PAPER"&tot3A$DT!="EDITORIAL"&tot3A$DT!="ERRATUM"&tot3A$DT!="SHORT SURVEY",] 
tot3B=tot3B[rowSums(is.na(tot3B)) != ncol(tot3B), ] #remove any rows with only NAs
tot3B$DT = factor(tot3B$DT)

## Restriction: only JOURNALS 
table(tot3B$PT)
tot3B$PT[is.na(tot3B$PT)]= "UNKNOWN"
tot3B = tot3B[tot3B$PT!="BOOK"&tot3B$PT!="BOOK SERIES"&tot3B$PT!="CONFERENCE PROCEEDING"&tot3B$PT!="TRADE JOURNAL",] 

## Restriction: max 20 authors per paper 
tot3 = tot3B[tot3B$nAU<21,] 
tot3=tot3[rowSums(is.na(tot3)) != ncol(tot3), ] #remove any rows with only NAs

### Save tot3 (all papers)

write.csv(tot3, "allpapers.csv")

## Restriction: only papers with at least one multi-author paper

## NOTE: Check manually if there are authors in your dataset that only have single author papers, and if so, remove them, as a network is not possible for these

## Add number of papers to 'df'
table = table(tot3$author.name) #create frequency table
table = as.data.frame(table)
tot.subset = subset(df, select = c("author.name"))
names(table) = c("author.name", "no.of.papers")
d = merge(tot.subset,table, by = "author.name", all = T) #1663
d = d[order(match(d$author.name, df$author.name)),]
df$no.of.papers <- d$no.of.papers #name of column
df$no.of.papers[df$no.of.papers==0] = NA #zeros don't make any sense, so change to NA

# safe df
write.csv(df, "fulllist.csv") 

### Make seperate dataset for every focal author and save as seperate csv files in /output_perauthorname

author.name = tot3$author.name
for(i in unique(author.name)){
  DF=tot3[tot3$author.name==i,]
  DF=DF[rowSums(is.na(DF)) != ncol(DF), ]
  DF=DF[!duplicated(DF),]
 write.csv(DF, row.names=FALSE,file.path("~/output_perauthorname",paste0(i,".output_perauthor", ".csv"))) #create file per author name
}

#The output here are a dataset per authorname (rather than ScopusID) in /output_perauthorname

## get number of papers for all focal authors
new=read_csv("complete.dataset.authors.csv") #NB: is only produced later in code

author.name.final= as.character(new$author.name)
author.name= as.character(tot3$author.name)
author.name = as.data.frame(author.name)

for(k in names(author.name)){d = ifelse(author.name[[k]] %in% author.name.final, 1, 0)}
tot3$final.authors = d 
table(tot3$final.authors)
tot3.final.authors = tot3[tot3$final.authors==1,]

### Look at the proportion of egonetworks other than its own a focal author is in 

authors = tot3.final.authors$author.name
#create empty datafile for structure
list.x <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(list.x) = c("x","author.name", "length", "n")

for(i in unique(authors)){
  tot3.final.authors$a = grepl(i, tot3.final.authors$AU) 
  a = tot3.final.authors[tot3.final.authors$a=="TRUE",]
  list = paste(unique(a$author.name))
  n = length(list)
  list = paste(unlist(list), collapse =" ")
  list=as.data.frame(list)
  list$author.name = i
  list$n=n
  list$p = n/length(unique(authors))
  names(list)= c("x","author.name", "n", "p")
  list.x = rbind(list.x,list)
}

# Fig ESM6A
A = ggplot(list.x, aes(x=p*100))+geom_histogram(binwidth = 0.4, colour = "black", fill = "white")+
  theme_bw()+xlab("% of other ego networks focal authors are included in") + ylab("Frequency")+
  theme(plot.margin=unit(c(.5,.5,.2,.5),"cm"))+
  theme(# Axis.text.y = element_blank(), #remove y axis tick labels
    axis.text.y = element_text(size=12, colour="black"), # Change size of y axis labels
    axis.text.x = element_text(size=12,colour="black"), # Change size of x axis numbers
    axis.title.x = element_text(size=12,vjust=-0.9,colour="black"), # Change size and font of x axis title and move it down a bit
    panel.grid.major = element_blank(), # Formatting to create blank plot with box around it
    panel.grid.minor = element_blank(), # Formatting to create blank plot with box around it
    axis.line = element_line(colour="black"),
    panel.background = element_rect(colour = "black", size=1, fill=NA),
    legend.key = element_rect(fill = "white", color = NA), # Make background of symbols in legend white
    legend.text=element_text(size=12),
    legend.title = element_text(size=12)) +
  geom_vline(xintercept = mean(list.x$p*100), color = "royalblue", size=1.5)


###Look at duplicate papers to check for non-independence, and back-up for standard statistical approach rather than permutations

#tot3 = read.csv("allpapers.csv")
table = table(tot3.final.authors$TI)
sum(is.na(tot3.final.authors$TI))
table = as.data.frame(table)
table$p = (table$Freq/938)*100

# Fig ESM6B
B = ggplot(table, aes(x=p))+geom_histogram(binwidth = 0.1, colour = "black", fill = "white")+labs(tag = "B")+
  theme_bw()+xlab("% of other ego networks focal authors are included in") + ylab("Frequency")+
  theme(plot.margin=unit(c(.5,.5,.2,.5),"cm"))+
  theme(# Axis.text.y = element_blank(), #remove y axis tick labels
    axis.text.y = element_text(size=12, colour="black"), # Change size of y axis labels
    axis.text.x = element_text(size=12,colour="black"), # Change size of x axis numbers
    axis.title.x = element_text(size=12,vjust=-0.9,colour="black"), # Change size and font of x axis title and move it down a bit
    panel.grid.major = element_blank(), # Formatting to create blank plot with box around it
    panel.grid.minor = element_blank(), # Formatting to create blank plot with box around it
    axis.line = element_line(colour="black"),
    panel.background = element_rect(colour = "black", size=1, fill=NA),
    legend.key = element_rect(fill = "white", color = NA), # Make background of symbols in legend white
    legend.text=element_text(size=12),
    legend.title = element_text(size=12)) +
  geom_vline(xintercept = mean(table$p), color = "royalblue", size=1.5)

grid.arrange(A,B,ncol=1)

############################################################################################
###			(3) Turn into publication*author matrix
############################################################################################
library(readr) 
library(asnipe) 
library(sna)
library(data.table) 
library(BBmisc) 

setwd("~/output_perauthorname")  
file_list <- list.files() 

for (file in file_list){
  subset <- read_csv(file)
  ## Split list of authors into seperate cells
  x <- strsplit(as.character(subset$AU), ";")
  l <- lengths(x)  
  m <- max(l)
  x <- t(sapply(x[as.logical(l)], function(a) c(a, rep("",m-length(a)))))
  x = as.data.frame(x)
  # Many names spelled slightly different. To reduce this error, we select first letter of first name + surname
  for(i in names(x)){
    x[[i]]= paste(sapply(strsplit(sub('^(\\w+).*\\b(\\w+)$', '\\1 \\2', as.character(x[[i]])), " "), function(r) paste0(substr(r[1], 1, 1), ".", substr(r[2], 1, 50)))) 
  }
  x[x=="NA.NA"]=""
  ## Remove duplicated author names
  x[t(apply(x, 1, duplicated))] <- NA 
  ## Turn into list for further analysis
  x.list <- as.list(as.data.frame(t(x))) #transpose to make list
  x.list=lapply(x.list, function(x) x[!is.na(x)]) #take out Nas in list
  x.list=lapply(x.list, function(x) x[(x!="")]) #take out Nas in list
  ## Get last author    
  dummy<- data.frame(matrix(ncol = 1, nrow = 1))
  names(dummy) = c("lastauthor")
  dummy$lastauthor = "DELETE"
  for(j in names(x.list)){
    lastauthor=tail(x.list[[j]], n=1)
    last = as.character(lastauthor)
    names(last)= c("lastauthor")
    dummy=rbind(dummy,last)
    lastauthor=as.data.frame(dummy[-1,])
    names(lastauthor)=c("lastauthor")
  }
  ## Identify first author    
  dummy2<- data.frame(matrix(ncol = 1, nrow = 1))
  names(dummy2) = c("firstauthor")
  dummy2$firstauthor = "DELETE"
  for(j in names(x.list)){
    firstauthor=head(x.list[[j]], n=1)
    first = as.character(firstauthor)
    names(first)= c("firstauthor")
    dummy2=rbind(dummy2,first)
    firstauthor=as.data.frame(dummy2[-1,])
    names(firstauthor)=c("firstauthor")
  }
  ## Turn into format: id (author=column) per group (paper=row)
  PSgbi <- get_group_by_individual(x.list, data_format="groups") 
  PSgbi=as.data.frame(PSgbi)
  PSgbi= PSgbi[,order(-colSums(PSgbi))] # order so that focal author is first
  ## Merge all with original dataset
  total =cbind(lastauthor,firstauthor, subset,PSgbi)
  ## Write csv file for each focal author
  write.csv(total, file.path("~/matrix",paste0(total$author.name[1],".matrix", ".csv")), row.names=FALSE) #create file per Scopus ID
}

#output: matrix file per author in /matrix folder

############################################################################################
###			(4) Get association matrix (ID*ID), egonet metrics and visualise social networks
############################################################################################
library(igraph) 
library(readr) 
library(sna) 
library(asnipe) 
#install.packages("http://cran.r-project.org/src/contrib/Archive/egonet/egonet_1.2.tar.gz", repos=NULL, type="source")
library(egonet) 
library(ggplot2) 

# Before you run this, make sure that the there are no authors with only single-author papers in ~/matrix, because the code won't work for these authors
setwd("~/matrix") 
file_list <- list.files() 

## Create empty dataframe for structure
ego <- data.frame(matrix(ncol = 14, nrow = 0))
colnames(ego) = c("effsize","constraint","outdegree","indegree","efficiency","hierarchy","centralization","gden","ego.gden","author.name","n.co.authors","clique.number","clusteringcoefficient.global", "triangles")

for (file in file_list){
  dataset <- read_csv(file)
  if(nrow(dataset)>2){ # implemented restriction: authors with less than 3 papers: can't make networks for those
    subset=subset(dataset,select = -c(1:33)) # only select matrix
    network = get_network(subset,data_format = "GBI",association_index = "HWI") #creates network on group_by_individual formatted data, each cell entry is the half-weight (recommended over simple ratio when comparing networks; Whitehead 2008 p103/104.) Also see p95)
    value = index.egonet(ceiling(network), ego.name=dataset$author.name[1]) #extract ego net measures
    egometrix = data.frame(t(value)) #save in dataframe
    egometrix$author.name = dataset$author.name[1] #add column with focal author name
    egometrix$n.co.authors = (ncol(dataset) - 35) #number of co-authors
    # Create igraph object
    g = graph.adjacency(network, mode = "undirected", weighted=TRUE, diag=FALSE) 
    egometrix$clusteringcoefficient.global = transitivity(g, type = "global") #Get glocal clustering coefficient. Info on: Local vs global clusteringcoefficients: https://toreopsahl.com/tag/clustering-coefficient/
    egometrix$triangles = sum(count_triangles(g, vids = V(g)))
    
    # Visualise social network:
    PScol = c("blue",rep("orange",(length(subset[1,])-1)))
    plot = plot(g, vertex.color = PScol, vertex.size=7, vertex.label=NA)	# plot without labels
    E(g)$width <- E(g)$weight*10 #add weighted edges based on simple ratio
    plot(g)
    
    # Save plot in ~/egonets folder (optional)
    #NOTE: remember to make 'egonets' folder in working directory first
    mypath <- file.path("~/egonets",paste("plot_", file,  ".jpg", sep = ""))
    ggsave(filename = mypath, plot = plot(g, vertex.color = PScol, vertex.size=3, vertex.label=NA))
    
    #ego <- rbind(ego,egometrix)
  }} 
ego = ego[!is.na(ego$author.name),] #takes out rows with only NAs
plot(ego$triangles/ego$clusteringcoefficient.global)
ego$demoninator = ego$triangles/ego$clusteringcoefficient.global
write.csv(ego, file.path("egonet_metrics.csv"), row.names=F)
#Output: all egometric metric per author in one csv file 'ego'
hist(ego$ego.gden)
length(ego$author.name) 

############################################################################################
###			(5) Get weighted edgelist, needed to get last egonet metric: tie strength
############################################################################################
library(intergraph) 
library(igraph) 
library(network) 

setwd("~/matrix") 
file_list <- list.files() 

## Create a new folder called 'edgelist' in the working directory before you run the following code
# The code in the following code was written and provided by James Curley
for (file in file_list){
  dataset <- read_csv(file)
  if(nrow(dataset)>2){ # Implemented restriction: authors with less than 3 papers: can't make networks for those
    mat <- subset(dataset,select = -c(1:33)) # only select group *IDmatrix
    m <- crossprod(as.matrix(mat))
    diag(m) <- 0
    m[upper.tri(m)]<-NA # If you only want one entry per dyad
    df <- reshape2::melt(m)
    colnames(df) <-c('SOURCE','TARGET', 'ASOC')
    df$AUTHOR.NAME <-dataset$author.name[1] 
    df<-df[df$SOURCE!=df$TARGET,]
    df=df[!is.na(df$ASOC),]
    write.csv(df, file.path("~/edgelist",paste0(dataset$author.name[1],".edgelist", ".csv")), row.names=FALSE) #create file per Scopus ID
  }}
# Output: weighted edgelist per author in /edgelist folder

### Now read in edgelists to get remainder of egometric measures 

setwd("~/edgelist") 
file_list <- list.files() 

## Create empty dataframe 
edge <- data.frame(matrix(ncol = 5, nrow = 1))
names(edge) = c("author.name","dyadcount","edgecount", "strength.mean", "strength.mean_repeat")

for (file in file_list){
  dataset <- read_csv(file)
  if(dataset$AUTHOR.NAME[1] %in% ego$author.name){ # Implemented restriction: authors with less than 3 papers: can't make networks for those
    dataset =  dataset[ dataset$ASOC>0,] #remove zero edges
    ego.alter = dataset[dataset$SOURCE==dataset$AUTHOR.NAME[1]|dataset$TARGET==dataset$AUTHOR.NAME[1],]
    strength.mean = mean(ego.alter$ASOC)
    dataset_repeat =  ego.alter[ego.alter$ASOC>1,] #remove zero edges
    strength.mean_repeat = mean(dataset_repeat$ASOC)
    gr <- graph_from_data_frame(dataset, directed= FALSE)
    net <- asNetwork(gr)
    dyadcount = network.dyadcount(net) #number of dyads
    edgecount = network.edgecount(net) #number of edges
    list = c(dataset$AUTHOR.NAME[1], dyadcount, edgecount, strength.mean, strength.mean_repeat)
    names(list) = c("author.name","dyadcount","edgecount","strength.mean", "strength.mean_repeat")
    edge= rbind(edge,list)
  }}

write.csv(edge[-1,], file.path("~/more_egonet_metrics.csv"), row.names=FALSE)


############################################################################################
###			(6) Retrieve publication years (first and last), and publications per year 
############################################################################################
library(readr) 
install.packages("http://cran.r-project.org/src/contrib/Archive/rowr/rowr_1.1.3.tar.gz", repos=NULL, type="source")
library(rowr) 

setwd("~/matrix") 
file_list <- list.files() 
length(file_list)

#create empty dataframe for structure
Empty <- data.frame(matrix(ncol = 0, nrow = 100))
rownames(Empty) = 1:100
Empty$year = 1:100
Empty2=as.data.frame(t(Empty))
Empty2$AUTHOR.NAME = ""
Empty2$first.year = ""
Empty2$last.year=""
Empty2$n.years=""
Empty2$first.lastauthor=""
Empty2$second.lastauthor=""
Empty2$third.lastauthor=""
Empty2$gap=""
Empty2$npapers.lastauthor=""
Empty2$Time.to.PI=""
Empty2$mean.nAU=""
Empty2$firstauthor.papers=""
Empty2$lastauthor.papers=""
Empty2$allmultipleauthor.papers=""
Empty2$singleauthor.papers=""

for (file in file_list){
  dataset <- read_csv(file)
  if(nrow(dataset)>2){# Implemented restriction: authors with less than 3 papers: can't make networks for those
    dataset$PY=as.numeric(as.character(dataset$PY))
    table=table(factor(dataset$PY,levels = min(dataset$PY):max(dataset$PY))) #get frequency of publication year
    table = as.data.frame(table)
    table$Var1=as.numeric(as.character(table$Var1))
    table$year=(table$Var1)-(table$Var1[1])+1 #this is the relative year, year 1 being the first year this author started publishing
    n.pub <- as.data.frame(table[c(3,2)]) #number of publications in that year
    x = merge(n.pub, Empty, all=TRUE)
    x = as.data.frame(t(x))#transpose so that each row is one focal author
    colnames(x) = x[1,] 
    x=x[2,]
    x$AUTHOR.NAME <-dataset$author.name[1] #add column with focal author name
    x$first.year = min(dataset$PY) #the first year the focal author published a paper
    x$last.year=max(dataset$PY) #the last year the focal author published a paper
    x$n.years=x$last.year-x$first.year+1 #publication years
    b = as.numeric(ifelse(dataset$lastauthor==x$AUTHOR.NAME&dataset$nAU>1, dataset$PY, "NONE")) #give year if focal author is last author on paper, only for multiple-author publications
    b =sort(b) #sort by year
    x$first.lastauthor = b[1]
    x$second.lastauthor = b[2] #select year of second last-author, multiple-author, publication. 
    x$third.lastauthor = b[3] #select year of second last-author, multiple-author, publication. 
    x$gap = x$third.lastauthor-x$first.lastauthor #publication gap between their first and third last-author papers 
    x$npapers.lastauthor = length(b) # number of papers on which the focal author was last author
    x$Time.to.PI= ifelse(!is.na(x$third.lastauthor)&x$npapers.lastauthor>2,(x$third.lastauthor-x$first.year), NA )  #Time to PI. Time between first publication year and year in which the focal author published the third paper on which (s)he was last author. At least three last paper authors. NA if never last author twice.                  
    x$mean.nAU = mean(dataset$nAU) #average degree per paper
    x$firstauthor.papers = sum(as.numeric(ifelse(dataset$firstauthor==dataset$author.name&dataset$nAU>1, 1, 0))) #number of multi-author papers the focal is first author on
    x$lastauthor.papers = sum(as.numeric(ifelse(dataset$lastauthor==dataset$author.name&dataset$nAU>1, 1, 0)))#number of multi-author papers the focal is last author on
    x$allmultipleauthor.papers = length(dataset[dataset$nAU>1,]$nAU) #total number of multi-authored papers
    x$singleauthor.papers  = length(dataset[dataset$nAU==1,]$nAU) #total number of single-authored papers
    Empty2 <- rbind(Empty2,x)
    rownames(Empty2) = NULL
    year.calculations=Empty2[-1,]
  }  }

write.csv(year.calculations, file.path("publications_per_year.csv"), row.names=F)

# Output: dataframe with (per author): no. of publications per year, first and last year,difference in years, first year as last author, difference between first publication and first paper as first paper

############################################################################################
###			(7) Merging dataframes and applying restrictions 
############################################################################################
library(lme4) 
library(ggplot2) 
library(readr) 
library(jtools)

## Read in ego metrics & merge
ego = read_csv("egonet_metrics.csv") 
ego = ego[!is.na(ego$author.name),] 
ego2=read_csv("more_egonet_metrics.csv") 
ego3=merge(ego,ego2,by= "author.name", all = T)

# Read in DF and merge with ego metrics
year.calculations = read_csv("publications_per_year.csv")[101:115] 
combo2 = cbind(ego3,year.calculations)
#Read in df and merge with all of the above
#add in df (original format)
df = read_csv("fulllist.csv") 
#select relevant columns
select.df = subset(df, select =  c("no.of.papers", "author.name", "Gender", "Country"))
select.df=select.df[!duplicated(select.df$author.name),]

new = merge(select.df,combo2, by = "author.name", all = FALSE) 
#NOTE: Check that data classes are correct in the 'new' dataframe.
new$n.years = as.numeric(new$n.years)
new$mean.nAU = as.numeric(new$mean.nAU)
new$first.year = as.numeric(new$first.year)
new$firstauthor.papers = as.numeric(new$firstauthor.papers)
new$lastauthor.papers = as.numeric(new$lastauthor.papers)
#Have a look at distribution first and last years, to look at outliers
hist(as.numeric(new$first.year))
hist(as.numeric(new$last.year))

### Applying restrictions

## 1. First publication 1980 or later
# First plot (Fig ESM3B)
ggplot(new[new$Gender!="UNK",], aes(x = first.year, colour = Gender,fill = Gender)) + geom_histogram(binwidth = 5, alpha = 0.4) +
  xlab("First publication year") + ylab("Frequency") +labs(tag = "B")+
  theme(plot.margin=unit(c(.5,.5,.2,.5),"cm"))+ theme_bw()+
  theme(# Axis.text.y = element_blank(), #remove y axis tick labels
    axis.text.y = element_text(size=12, colour="black"), # Change size of y axis labels
    axis.text.x = element_text(size=12,colour="black"), # Change size of x axis numbers
    axis.title.x = element_text(size=12,vjust=-0.9,colour="black"), # Change size and font of x axis title and move it down a bit
    panel.grid.major = element_blank(), # Formatting to create blank plot with box around it
    panel.grid.minor = element_blank(), # Formatting to create blank plot with box around it
    axis.line = element_line(colour="black"),
    panel.background = element_rect(colour = "black", size=1, fill=NA),
    legend.position = c(0.1,0.7), # Position legend inside plot
    legend.background = element_rect(fill="transparent"),
    legend.key = element_rect(fill = "white", color = NA), # Make background of symbols in legend white
    legend.text=element_text(size=12),
    legend.title = element_text(size=12)) +
  geom_vline(xintercept = 1980, color = "royalblue", size=1.5)+
  scale_color_manual(values=c("black", "black"))+
  scale_fill_manual(values=c("black", "grey"))

new = new[new$first.year>1979,] 
x =(new[!is.na(new$first.year),])
length(unique(x$author.name)) #944
#1136-944=192 authors out
#192/1136= 17% out
hist(new$first.year)

## 2. Only include focal authors with 400 or less papers (big outliers)
# First plot (Fig ESM3A)
ggplot(new[new$Gender!="UNK",], aes(x = no.of.papers, colour = Gender,fill = Gender)) + geom_histogram(binwidth = 0.25, alpha = 0.4) + scale_x_log10()+  
  xlab("Number of papers/author (log scale)") + ylab("Frequency") +labs(tag = "A")+
  theme(plot.margin=unit(c(.5,.5,.2,.5),"cm"))+ theme_bw()+
  theme(axis.text.y = element_text(size=12, colour="black"), 
    axis.text.x = element_text(size=12,colour="black"), 
    axis.title.x = element_text(size=12,vjust=-0.9,colour="black"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour="black"),
    panel.background = element_rect(colour = "black", size=1, fill=NA),
    legend.position = c(0.1,0.7), 
    legend.key = element_rect(fill = "white", color = NA), 
    legend.text=element_text(size=12),
    legend.background = element_rect(fill="transparent"),
    legend.title = element_text(size=12)) +
  geom_vline(xintercept = 400, color = "royalblue", size=1.5)+
  scale_color_manual(values=c("black", "black"))+
  scale_fill_manual(values=c("black", "grey"))

new = new[new$no.of.papers<401,]	
new=new[rowSums(is.na(new)) != ncol(new),] #remove all NA rows

## Anonymise data 
#length(new[,1])
#new$author.name = sample(1:length(new[,1]), length(new[,1]), replace=FALSE)
#new = subset(new, select = -AUTHOR.NAME )

#take out UNK gender
new = new[new$Gender!="UNK",]
## save this datafile! 
write.csv(new, file.path("social.authors.data.csv"), row.names=FALSE)

############################################################################################
###			(8) Calculating career gaps 
############################################################################################
library(lme4)
library(ggplot2)
library(readr)
library(reshape2)
library(rowr)
library(qpcR)
library(reshape2)
library(plyr)

new = read_csv("social.authors.data.csv")
year.calculations = read_csv("publications_per_year.csv")
colnames(year.calculations)[101] <- 'author.name'

DF = merge(year.calculations, new,by = c("author.name"), all=FALSE)
#calculate gaps
DF = DF[2:39]
DF[is.na(DF)] = 0
DF[DF==0] = ""

#create empty dataframe for structure
Empty <- data.frame(matrix(ncol = 1, nrow = 100))
names(Empty) = "final"

for(i in rownames(DF)){
  x <- DF[i,] 
  x = x[, colSums(x != "") != 0]
  diffs <- x[-1L] != x[-length(x)]
  idx <- c(which(diffs), length(x))
  diff=(as.data.frame(diff(c(0, idx)) -1))
  names(diff)="x"
  diff=diff[diff$x!=0,]
  diff=as.data.frame(diff)
  final2 = qpcR:::cbind.na(diff, Empty)[,1]
  final2=as.data.frame(final2)
  Empty=cbind(Empty,final2)
}

Empty0 = Empty[,-1]
Empty.t = as.data.frame(t(Empty0)) #list of all the gap durations per author
Empty.t = Empty.t[,1:10] #because max 10 gaps
Empty.t.G =cbind(Empty.t,new$Gender, new$author.name)

names(Empty.t.G)=c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","Gender", "author.name")
mean(!is.na(Empty.t))

Empty.t.melt = melt(Empty.t.G, id= c( 'Gender', "author.name")) 
Empty.t.melt = Empty.t.melt[!is.na(Empty.t.melt$value),]

hist(Empty.t.melt$value, xlab = "all career gap lengths", main="")

max(Empty.t.G$value)
Empty.t.melt$variable = as.character(Empty.t.melt$variable)
head(Empty.t.melt)

x = ddply(Empty.t.melt[Empty.t.melt$Gender!="UNK",],  c("Gender","author.name"),summarise,mean = mean(value, na.rm = TRUE))

#Fig. ESM8A
ggplot(x, aes(x = mean,colour = Gender,fill = Gender)) + geom_histogram(binwidth = 2,origin = 0.05, alpha = 0.4) +  labs(tag = "A")+
  xlab("Mean career gap length (years)") + ylab("Frequency") +
  theme(plot.margin=unit(c(.5,.5,.2,.5),"cm"))+
  theme(# Axis.text.y = element_blank(), #remove y axis tick labels
    axis.text.y = element_text(size=12, colour="black"), # Change size of y axis labels
    axis.text.x = element_text(size=12,colour="black"), # Change size of x axis numbers
    axis.title.x = element_text(size=12,vjust=-0.9,colour="black"), # Change size and font of x axis title and move it down a bit
    panel.grid.major = element_blank(), # Formatting to create blank plot with box around it
    axis.line = element_line(colour="black"),
    panel.background = element_rect(colour = "black", size=1, fill=NA),
    legend.position = c(0.9,0.8), # Position legend inside plot
    legend.key = element_rect(fill = "white", color = NA), # Make background of symbols in legend white
    legend.text=element_text(size=12),
    legend.background = element_rect(fill="transparent"),
    legend.title = element_text(size=12)) +
  geom_vline(xintercept = 2, color = "royalblue", size=1.5)+
  scale_color_manual(values=c("black", "black"))+
  scale_fill_manual(values=c("black", "grey"))
table(currentgap$currentgap)

#calculate last gap  
dummy<- data.frame(matrix(ncol = 1, nrow = 1))
names(dummy) = c("gap")
for(j in (x.list)){
  tryCatch({ 
    gap=ifelse(is.logical(tail(j, n=1)), print(NA), tail(j, n=1))
    last = as.data.frame(gap)
    names(last)= c("gap")
    dummy=rbind(dummy,last)
  }, warning = function(w){})
}

dummy=as.data.frame(dummy[(-1),])
names(dummy)= "gap"

#load new from Social authors code 
new = read_csv("social.authors.data.csv")

all = cbind(Empty.t, new, dummy)

all$currentgap = 2018- all$last.year
currentgap = all[all$currentgap>0,]
currentgap$Gender = as.factor(currentgap$Gender)
currentgap = currentgap[currentgap$Gender!="UNK",]
currentgap = currentgap[!is.na(currentgap$Gender),]
hist(currentgap$currentgap, xlab="Time since last publication (years)")
abline(v =2,col = "royalblue",lwd = 2)
str(currentgap)
currentgap = currentgap[!duplicated(colnames(currentgap))]

ggplot(currentgap, aes(x = currentgap,colour = Gender,fill = Gender)) + geom_histogram(binwidth = 2, alpha = 0.4) +   labs(tag = "B")+
  xlab("Time since last publication (years)") + ylab("Frequency") +
  theme(plot.margin=unit(c(.5,.5,.2,.5),"cm"))+
  theme(# Axis.text.y = element_blank(), #remove y axis tick labels
    axis.text.y = element_text(size=12, colour="black"), # Change size of y axis labels
    axis.text.x = element_text(size=12,colour="black"), # Change size of x axis numbers
    axis.title.x = element_text(size=12,vjust=-0.9,colour="black"), # Change size and font of x axis title and move it down a bit
    panel.grid.major = element_blank(), # Formatting to create blank plot with box around it
    axis.line = element_line(colour="black"),
    panel.background = element_rect(colour = "black", size=1, fill=NA),
    legend.position = c(0.9,0.8), # Position legend inside plot
    legend.key = element_rect(fill = "white", color = NA), # Make background of symbols in legend white
    legend.text=element_text(size=12),
    legend.background = element_rect(fill="transparent"),
    legend.title = element_text(size=12)) +
  geom_vline(xintercept = 3, color = "royalblue", size=1.5)+
  scale_color_manual(values=c("black", "black"))+
  scale_fill_manual(values=c("black", "grey"))

############################################################################################
###			(9) Calculating social network metrics for first 10 ten years 
############################################################################################

# To calculate social network metrics over the first 10 years 
# for focal authors who published their first paper within one year of first participating at an ISBE conference we:
# 1. Identified first year of publication (df$first.year)
# 2. Identified year of first publication contribution(df$first.conference)
# 3. Compiled a list for who: df$first.year>=(df$first.conference-1):
# df$include = ifelse(df$first.year>=(df$first.conference-1),1,0)
# 4. Reran the above code for the authors for which df$include==1:
# if(nrow(dataset)>2&dataset$author.name[1] %in% sum$author.name[sum$include==1]){ # implemented restriction: authors with less than 3 papers: can't make networks for those
# first = min(dataset$PY)
# last = first + 9
# dataset1 = subset(dataset[dataset$PY<=last,])
# 5. Merged the new dataframe 'tot' with 'social.authors.data.csv' to obtain career progression measures:
# new = read_csv("~/social.authors.data.csv")
# new = merge(subset(tot,select=c("author.name", "Gender","indegree","strength.mean","clusteringcoefficient.global", "no.of.papers", "demoninator", "triangles")),subset(new,select=c("author.name","n.years","last.year")), by = "author.name", all.x = TRUE)
# 6. Run the code in Part 2