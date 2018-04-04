library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

setwd("/home/ali/PDFs")


# Read the text file from internet
filePath <- "http://www.americanrhetoric.com/barackobamaspeeches.htm"

#leer los url de los pdf
urls <- c("http://www.americanrhetoric.com/speeches/PDFFiles/Barack%20Obama%20-%202004%20DNC%20Address.pdf", "http://www.americanrhetoric.com/speeches/PDFFiles/Barack%20Obama%20-%20Senate%20Speech%20on%20Ohio%20Electoral%20Vote.pdf", "http://www.americanrhetoric.com/speeches/PDFFiles/Barack%20Obama%20-%20Knox%20College%20Commencement.pdf")
for (url in urls) {
  download.file(url, destfile = basename(url))
}
download.file(url, destfile = basename(url), method="curl", extra="-k")
#get url de pdf para pasar a txt
myfiles <- list.files(path = "/home/ali/PDFs", pattern = "pdf", full.names = TRUE)
myfiles
sapply(myfiles, FUN = function(i){
  file.rename(from = i, to =  paste0(dirname(i), "/", gsub(" ", "", basename(i))))
})
myfiles <- list.files(path = url, pattern = "pdf", full.names = TRUE)
myfiles
lapply(myfiles[0], function(i){
  # convert pdf to ppm (an image format), just pages 1-10 of the PDF
  # but you can change that easily, just remove or edit the 
  # -f 1 -l 10 bit in the line below
  shell(shQuote(paste0("pdftoppm ", i, " -f 1 -l 10 -r 600 ocrbook")))
  # convert ppm to tif ready for tesseract
  shell(shQuote(paste0("convert *.ppm ", i, ".tif")))
  # convert tif to text file
  shell(shQuote(paste0("tesseract ", i, ".tif ", i, " -l eng")))
  # delete tif file
  file.remove(paste0(i, ".tif" ))
})

url
lapply(myfiles, function(i) system(paste('"/home/ali/PDFs/txt.txt"', paste0('"', i, '"')), wait = FALSE) )
myfiles
#leer archivos txt
mytxtfiles <- list.files(path = "/home/ali/PDFs", pattern = "txt",  full.names = TRUE)
mytxtfiles

tmpText <- data.frame(c(mytxtfiles))
tmpText
ds <- DataframeSource(tmpText)
corp <- Corpus(ds)
corp <- Corpus(DirSource(url, pattern = "txt"))
# warnings may appear after you run the previous line, they
# can be ignored
corp <- tm_map(corp,  removeNumbers)
corp <- tm_map(corp,  removePunctuation)
corp <- tm_map(corp,  stripWhitespace)
mydtm <- DocumentTermMatrix(corp)
# remove some OCR weirdness
# words with more than 2 consecutive characters
mydtm <- mydtm[,!grepl("(.)\\1{2,}", mydtm$dimnames$Terms)]

# get each doc as a csv with words and counts
for(i in 1:nrow(mydtm)){
  # get word counts
  counts <- as.vector(as.matrix(mydtm[1,]))
  # get words
  words <- mydtm$dimnames$Terms
  # combine into data frame
  df <- data.frame(word = words, count = counts,stringsAsFactors = FALSE)
  # exclude words with count of zero
  df <- df[df$count != 0,]
  # write to CSV with original txt filename
  write.csv(df, paste0(mydtm$dimnames$Docs[i],".csv"), row.names = FALSE) 
}
abstracts <- lapply(mytxtfiles, function(i) {
  j <- paste0(scan(i, what = character()), collapse = " ")
  regmatches(j, gregexpr("(?<=Abstract).*?(?=Introduction)", j, perl=TRUE))
})
lapply(1:length(abstracts), function(i) write.table(abstracts[i], file=paste(mytxtfiles[i], "abstract", "txt", sep="."), quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " " ))

url
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=Inf, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

