
---
title: "Data Preprocessing"
#author: "Marta Fernandes"
output:
  html_document:
    fig.align: center
    fig_caption: yes
    fig_height: 5
    fig_width: 9
    highlight: tango
    theme: united
    toc: yes
  pdf_document:
    toc: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# Data integration

## Exercise


Aim: This exercise involves combining the separate output datasets exported from separate MIMIC queries into a consistent larger dataset table.

To ensure that the associated observations or rows from the different datasets match up, the right column ID must be used. For example, in MIMIC subject_id is used to identify each individual patient, so includes their date of birth (DOB), date of death (DOD) and various other clinical detail and laboratory values. Likewise, the hospital admission ID, hadm_id, is used to specifically identify various events and outcomes from an unique hospital admission; and is also in turn associated with the subject_id of the patient who was involved in that particular hospital admission. Tables pulled from MIMIC can have one or more ID columns. The different tables exported from MIMIC may share some ID columns, which allows us to 'merge' them together, matching up the rows correctly using the unique ID values in their shared ID columns.

Source: MIMIC III demo version, https://physionet.org/works/MIMICIIIClinicalDatabaseDemo/files/

To demonstrate this with MIMIC data, a simple SQL query is constructed to extract some data from: "admissions.csv" and "icustays.csv". We will use these extracted files to show how to merge datasets in R.

1. SQL queries:

Extract the datasets with each one of the following lines of code in SQL:

```{r message=FALSE, warning=FALSE}

## SELECT * FROM admissions 

## SELECT * FROM icustays

```

Save admissions.csv and icustays.csv in your desktop folder: "MIMIC_data_files" 

2. R code: Demonstrating data integration

Merging admissions and icustays: to get the rows to match up correctly, we need to merge on both the *subject_id* and *hadm_id*. This is because each subject/patient could have multiple *hadm_id* from different hospital admissions during the EHR course of the MIMIC database.

Set working directory and read data files into R:

```{r message=FALSE, warning=FALSE}

work_dir <- "C:/Users/marta/Desktop/MIMIC_data_files" # here your directory

setwd(work_dir)

adm <- read.csv(file="admissions.csv", header=TRUE, sep =",")

icu <- read.csv(file="icustays.csv", header=TRUE, sep =",")

icu_adm  <- merge(adm, icu, by = c("SUBJECT_ID", "HADM_ID"))

head(icu_adm, n =3L)

```

Note: in the same way each subject/patient could have multiple *hadm_id* from different hospital admissions during the EHR course of the MIMIC database, each *hadm_id* can have multiple *icustay_id*.


# Data transformation

## Exercise

Aim: To transform the presentation of data values in some ways so that the new format is more suitable for the subsequent statistical analysis. The main processes involved are normalization, aggregation and generalization.

Source: 
MIMIC III demo version, https://physionet.org/works/MIMICIIIClinicalDatabaseDemo/files/


1. SQL queries:

Extract the dataset by running the following code in SQL:

```{r message=FALSE, warning=FALSE}
##Elixhauser scores: https://github.com/MIT-LCP/mimic-code/blob/master/concepts/comorbidity/elixhauser-ahrq-v37-with-drg.sql
```

Save Elixhauser.csv in your desktop folder: "MIMIC_data_files".


2. R code: Demonstrating data transformation

Note the total number of rows (obs) and columns (variables) in Elixhauser table:

```{r message=FALSE, warning=FALSE}

work_dir <- "C:/Users/marta/Desktop/MIMIC_data_files"

setwd(work_dir)

Elixhauser<- read.csv(file="Elixhauser.csv", header=TRUE, sep =",")

str(Elixhauser)

```
Here we add a column in Elixhauser table to save the *"Elixhauser_overall"* in new table scores:

```{r message=FALSE, warning=FALSE}

scores <- cbind(Elixhauser, rep(0, nrow(Elixhauser)))
colnames(scores)[ncol(scores)] <- "Elixhauser_overall"

str(scores)

```

Note the total number of rows (obs) and columns (variables) and the new column *Elixhauser_overall*.     

###Aggregation step

Aim: To sum up the values of all the Elixhauser comorbidities across each row. 

```{r message=FALSE, warning=FALSE}

scores$Elixhauser_overall <- rowSums( scores[,3:32])
```
Let's take a look at the head of the resulting first and last column:

```{r message=FALSE, warning=FALSE}

head(scores[, c(1,33)])

```


###Normalization Step

Aim: To scale values in column Elixhauser_overall between 0 and 1, i.e. in [0, 1]. Function max() finds the maximum value in column *Elixhauser_overall*. Then we re-assign each entry in column *Elixhauser_overall* as a proportion of the max_score to normalize/scale the column.


```{r message=FALSE, warning=FALSE}

max_score <- max(scores$Elixhauser_overall)

scores$Elixhauser_overall <- scores$Elixhauser_overall/max_score
```

We subset and remove all the columns in Elixhauser, except for *subject_id*, *hadm_id* and *Elixhauser_overall*:

```{r message=FALSE, warning=FALSE}

scores <- scores[,c(1,2,33)]

```

###Generalization Step

Aim: Consider only the group of patients sicker than the average Elixhauser score. The function which() returns the row numbers (indices) of all true entries of the logical condition set on scores inside the round() brackets, where the condition being the column entry for *Elixhauser_overall* >= 0.5. We store the row indices information in the vector, 'sicker'. Then we can use 'sicker' to subset scores and only select rows/patients who are 'sicker' and store this information in *'scores_sicker'*.

```{r message=FALSE, warning=FALSE}

sicker <- which(scores$Elixhauser_overall>=0.5)

score_sicker <- scores[sicker,]
  

head(score_sicker)
```

Save the results to file: we can use e.g. write, table() and write.csv(). We give an example here:

```{r message=FALSE, warning=FALSE}

work_dir <- "C:/Users/marta/Desktop/MIMIC_data_files"

setwd(work_dir)

write.table(score_sicker, file = 'score_sicker.csv', sep =",")

```

#Data reduction

##Exercise

Aim: To reduce or reshape the input data by means of a more effective representation of the dataset without compromising the integrity of the original data. One element of data reduction is eliminating redundant records while preserving needed data, which we will demonstrate in Part 1. The other element involves reshaping the dataset into a "tidy" format, which we will demonstrate in Part 2.

Source: MIMIC III demo version, https://physionet.org/works/MIMICIIIClinicalDatabaseDemo/files/

###Part 1: Eliminating Redundant Records

To demonstrate this with an example, we will look at multiple records of glucose laboratory values for each patient. We will use the records from the following SQL query, which we exported as "labs_glucose.csv". The SQL query selects all the non-null measurements of glucose values for all the patients in the MIMIC database.

1. SQL query:

```{r message=FALSE, warning=FALSE}


##SELECT * FROM labevents WHERE (itemid = 50931 OR itemid = 50809)  
##AND valuenum IS NOT null AND hadm_id IS NOT null

```
Save labs_glucose.csv in your desktop folder: "MIMIC_data_files" 

2. R code: Demonstrating data reduction

There are a variety of methods that can be chosen to aggregate records. In this case we will look at averaging multiple glucose records into a single average glucose for each patient. Other options which may be chosen include using the first recorded value, a minimum or maximum value, etc. For a basic example, the following code demonstrates data reduction by averaging all of the multiple records of glucose into a single record per patient hospital admission. The code uses the aggregate() function:

```{r message=FALSE, warning=FALSE}

work_dir <- "C:/Users/marta/Desktop/MIMIC_data_files"

setwd(work_dir)

all_glucose <- read.csv(file="labs_glucose.csv", header=TRUE, sep =",")

str(all_glucose)


```

This step averages the glucose values for each distinct *hadm_id*:

```{r message=FALSE, warning=FALSE}

avg_glucoses <- aggregate(all_glucose, by=list(all_glucose$X.valuenum.), FUN=mean,
na.rm=TRUE)

head(avg_glucoses)

```

###Part 2: Reshaping Dataset

Ideally, we want a "tidy" dataset reorganized in such a way so it follows these 3 rules:

* 1. Each variable forms a column
* 2. Each observation forms a row
* 3. Each value has its own cell

Datasets exported from MIMIC usually are fairly "tidy" already. Therefore, we will construct our own data frame here for ease of demonstration for rule 3. We will also demonstrate how to use some common data tidying packages.

R code: To mirror our own MIMIC dataframe, we construct a dataset with a column of *subject_id* and a column with a list of diagnoses for the admission.

```{r message=FALSE, warning=FALSE}

diag <- data.frame(subject_id = 1:6, diagnosis = c("PNA, CHF", "DKA", "DKA, UTI", "AF, CHF", "AF", "CHF"))

diag

```

Note that the dataset above is not "tidy". There are multiple categorical variables in column "diagnosis" - breaks "tidy" data rule 1. There are multiple values in column "diagnosis" - breaks "tidy" data rule 3. There are many ways to "tidy" and reshape this dataset. We will show one way to do this by making use of R packages "splitstackshape" and "tidyr" to make reshaping the dataset easier.

#### R package example 1-"splitstackshape":

Installing and loading the package into R console.

```{r message=FALSE, warning=FALSE}

# install.packages("splitstackshape") -- uncomment this line

library(splitstackshape)

```

The function, cSplit(), can split the multiple categorical values in each cell of column "diagnosis" into different columns, "diagnosis_1" and "diagnosis_2". If the argument, direction, for cSplit() is not specified, then the function splits the original dataset "wide".

```{r message=FALSE, warning=FALSE}

diag2 <- cSplit(diag, "diagnosis", ",")

diag2

```

One could possibly keep it as this if one is interested in primary and secondary diagnoses (though it is not strictly "tidy" yet). Alternatively, if the direction argument is specified as "long", then cSplit splits the function "long" like so:

```{r message=FALSE, warning=FALSE}

diag3 <- cSplit(diag, "diagnosis", ",", direction = "long")

diag3

```

Note diag3 is still not "tidy" as there are still multiple categorical variables under
column diagnosis-but we no longer have multiple values per cell. 

#### R package example 2-"tidyr":

To further "tidy" the dataset, package "tidyr" is pretty useful.

```{r message=FALSE, warning=FALSE}

# install.packages("tidyr") -- uncomment this line

library(tidyr)

```

The aim is to split each categorical variable under column *diagnosis* into their own columns with 1 = having the diagnosis and 0 = not having the diagnosis. To do this we first construct a third column, "yes", that hold all the 1 values initially (because the function we are going to use requires a value column that corresponds to the multiple categories column we want to 'spread' out).

```{r message=FALSE, warning=FALSE}

diag3$yes <- rep(1, nrow(diag3))

diag3

```

Then we can use the spread function to split each categorical variables into their own columns. The argument, fill = 0, replaces the missing values.

```{r message=FALSE, warning=FALSE}

diag4 <- spread(diag3, diagnosis, yes, fill = 0)
diag4

```

One can see that this dataset is now "tidy", as it follows all three "tidy" data rules.
