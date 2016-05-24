library(RPostgreSQL)
library(tm)
library(data.table)

# Connect with postgreSQL to a database that consists of Google's Spanish ngram data.
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="ngram", host="localhost", port=5432, user = "username", password = "password")

# Process a folder of text files and create database tables for 
docs <- Corpus(DirSource("../../corpus/corpus/"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, PlainTextDocument)

novel <- as.character(docs[[DOCNUMBERHERE]])
novel_list <- strsplit(novel, "\\W")
novel_vector <- unlist(tl)
novel_tbl <- table(tv[which(tv!="")])
novel_df <- as.data.frame(mtbl)
colnames(novel_df) <- c("word", "count")

novelcreatetable <- dbWriteTable(con, name = "novel_name", value = novel_df, row.names = FALSE, overwrite = TRUE)

query <- paste("SELECT COUNT(ngram), dates
               FROM (SELECT ngram, MIN(year) AS dates FROM total 
               JOIN --NOVELTABLE-- ON word = ngram GROUP BY ngram HAVING MIN(year) > 1800) AS dates
               GROUP BY dates ORDER BY dates")

novel_year_counts <- dbGetQuery(con, query)