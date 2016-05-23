library(tm)
library(qdap)
library(data.table)


# Return word frequencies in one of three formats from a plain text file. (Adapted from Jockers, Text Analysis for Students of Literature)

getTxtFreqs <- function(txtfile, raw=FALSE, rel=TRUE, vec=FALSE){
  text.v <- scan(txtfile, what="character", sep="\n")
  novel.v <- paste(text.v, collapse=" ")
  novel.lower.v <- tolower(novel.v)
  novel.words.l <- strsplit(novel.lower.v, "\\W")
  novel.words.v <- unlist(novel.words.l)
  novel.freqs.t <- table(novel.words.v[which(novel.words.v!="")])
  novel_rel_freqs_table <- (novel.freqs.t/sum(novel.freqs.t))
  if(raw==TRUE)  {
    return(novel.freqs.t) 
  }
  else if(vec==TRUE){
    return(novel.words.v[which(novel.words.v!="")])
  }
  else if(rel==TRUE){
    return(novel_rel_freqs_table)
  }
  return(novel_rel_freqs_table)
}

# Find keywords of novels in a directory based on chi squared contingency tables

keywordsFromDir <- function(directoryName, stopwordFile, p = .05){
  stopwordFile <- scan(stopwordFile, what = "character")
  novelKeywordList <- list()
  corp <- Corpus(DirSource(directoryName))
  corpora_wfm <- as.wfm(corp)
  colnames(corpora_wfm) <- names(corp)
    for(n in 1:length(corp)){
      text_v <- corpora_wfm[,n]
      ref_v <- rowSums(corpora_wfm[,-n])
      novelKeywordList[[names(corp[n])]] <- keywordsOneText(text_v, ref_v, stopwordFile = stopwordFile, p)
    }
  return(novelKeywordList)
}

# Helper to above gets the individual keywords for each separate text.

keywordsOneText <- function(text_v, ref_v, stopwordFile, p){
    wordList <- names(which(text_v>0))
    wordList <- rm_stopwords(wordList, stopwordFile, unlist = TRUE)
    keywordList <- list()
    pvalues <- list()
    
    
    for (i in 1:length(wordList)){
      textword <- wordList[i]
      conTbl <-  rbind(c(ref_v[textword], sum(ref_v)), c(text_v[textword], sum(text_v)))
      conTbl[is.na(conTbl)] <- 0
      test <- chisq.test(conTbl)
      remove(conTbl)
      pvalues <- rbind(pvalues, data.frame(textword, test$p.value))
    }
    keywordList <- pvalues[which(pvalues$test.p.value < p),]
    kv <- as.character(keywordList[,1])
    tv <- text_v[kv]
    return(tv)

}



