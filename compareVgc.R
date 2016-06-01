library(zipfR)

# Create a directory of tfl files given a directory of plain text files

createTfls <- function(directoryName){
  files_v <- dir(path=directoryName, pattern="*") 
  for(i in 1:length(files_v)){
    raws.l <- as.data.frame(getTxtFreqs(file.path(directoryName, files_v[i]), raw = TRUE))
    name <- paste(files_v[i], "tfl", sep="." )
    name <- paste("tfls/", name, sep="")
    names(raws.l) <- c("type", "f")
    write.table(raws.l, file = name, sep="\t", col.names = NA)
  }
}

# Compare the vocabulary growth curves of two tfl files given their file paths

compareVgc <- function(a_path, b_path){
  library(zipfR)
  a_tfl <- read.tfl(a_path)
  b_tfl <- read.tfl(b_path)
  b_spc <- tfl2spc(b_tfl)
  a_spc <- tfl2spc(a_tfl)
  b.fzm <- lnre("fzm", b_spc, exact=FALSE)
  a.fzm <- lnre("fzm", a_spc, exact=FALSE)
  a.fzm.vgc <- lnre.vgc(a.fzm, (1:230000), variances=TRUE)
  b.fzm.vgc <- lnre.vgc(b.fzm, (1:230000), variances=TRUE)
  comparisonList <- list("a" = a.fzm.vgc, "b" = b.fzm.vgc)
  return(comparisonList)
}

# Produce a list of vocabulary growth objects for all files from a directory of TFL files
compareFzmDir <- function(directoryName){
  comparisonList <- list()
  files_v <- dir(path=directoryName, pattern="*") 
  for(i in 1:length(files_v)){
    a_tfl <- read.tfl(file.path(directoryName, files_v[i]))
    a_spc <- tfl2spc(a_tfl)
    a.fzm <- lnre("fzm", a_spc, exact=FALSE)
    a.fzm.vgc <- lnre.vgc(a.fzm, (1:500000), variances=TRUE)
    comparisonList[i] <- list(a.fzm.vgc)
    
  }
  name <- gsub("\\_.*", "", files_v)
  names(comparisonList) <- name
  return(comparisonList)
}

# Poduce a list of vocabulary spectrums from a directory of tfl files

compareSpcDir <- function(directoryName){
  comparisonList <- list()
  files_v <- dir(path=directoryName, pattern="*") 
  for(i in 1:length(files_v)){
    a_tfl <- read.tfl(file.path(directoryName, files_v[i]))
    a_spc <- tfl2spc(a_tfl)
    comparisonList[i] <- list(a_spc)
    
  }
  name <- gsub("\\_.*", "", files_v)
  names(comparisonList) <- name
  return(comparisonList)
}

# From a directory of plain text files, get the ratio of words used only once to total words

hapaxRatio <- function(directoryName){
  spcList <- compareSpcDir(directoryName)
  hapaxRatioDf <- data.frame(Hapax=as.numeric(), Total=as.numeric())
  for (i in 1:length(spcList)){
    hapaxRatioDf <- rbind(hapaxRatioDf, c(N(spcList[[i]]), Vm(spcList[[i]], 1)))
  }
  rownames(hapaxRatioDf) <- names(spcList)
  return(hapaxRatioDf)
}



compareVgcDir <- function(directoryName, chunk_size = 500){
  comparisonList <- list()
  files_v <- dir(path=directoryName, pattern="*") 
  for(i in 1:length(files_v)){
    novel <- tolower(scan(file.path(directoryName, files_v[i]), what = "character"))
    novel_growth <- growth.fnc(text = novel, size = chunk_size) 
    novel_vgc <- growth2vgc.fnc(novel_growth)
    comparisonList[[i]] <- novel_vgc
  }
  name <- gsub("\\_.*", "", files_v)
  names(comparisonList) <- name
  return(comparisonList)
}
library(languageR)

getVgc <- function(filePath, size = 500, vgc = TRUE) {
  novel <- tolower(scan(filePath, what = "character"))
  novel_growth <- growth.fnc(text = novel, size = size) 
  novel_vgc <- growth2vgc.fnc(novel_growth)
  if (vgc == TRUE)
    return(novel_vgc)
  else
    return(novel_growth)
}

compareVgrDir <- function(directoryName, chunk_size = 500, nchunks = 100){
  comparisonList <- list()
  files_v <- dir(path=directoryName, pattern="*") 
  for (i in 1:length(files_v)){
    novel <- tolower(scan(file.path(directoryName, files_v[i]), what = "character"))
    novel_growth <- growth.fnc(text = novel, size = chunk_size, nchunks = nchunks) 
    comparisonList[[i]] <- novel_growth
  }
  name <- gsub("\\_.*", "", files_v)
  names(comparisonList) <- name
  return(comparisonList)
}

plotVgrDir <- function(directoryName, chunk_size = 500, nchunks = 100){
  comparisonList <- compareVgrDir(directoryName, chunk_size, nchunks)
  for(i in 1:length(comparisonList)){
    novelData <- comparisonList[[i]]@data$data
    novelGrRt <- novelData$HapaxLegomena/novelData$Tokens
    if (i == 1){
      plot(
        novelData$Tokens, 
        novelGrRt, 
        pch = 14 +i,
        col = i
        )
    }
    points(
      novelData$Tokens, 
      novelGrRt, 
      pch = 14 + i,
      col = i
    )
    Sys.sleep(0)
  }
  legend(
    x = "topright", 
    legend = names(comparisonList), 
    pch = c((1:length(comparisonList))+14),
    col = c(1:length(comparisonList)),
    cex = 1,
    ncol = 2,
    bty = "n",
    bg = "none",
    xjust = 1,
    y.intersp = .5,
    x.intersp = .5
    )
}








#maria.vgc <- getVgc("../../corpus/corpus/maria_isaacs_colombia_1867.txt"2)
