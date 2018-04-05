library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
#establecemos la carpeta de trabajo
home <-"/home/ali/PDFs"
#establecemos el espacio de trabajo
setwd(home)
#creamos un array de urls
urls <- c("http://www.americanrhetoric.com/speeches/PDFFiles/Barack%20Obama%20-%202004%20DNC%20Address.pdf", "http://www.americanrhetoric.com/speeches/PDFFiles/Barack%20Obama%20-%20Senate%20Speech%20on%20Ohio%20Electoral%20Vote.pdf", "http://www.americanrhetoric.com/speeches/PDFFiles/Barack%20Obama%20-%20Knox%20College%20Commencement.pdf")
#descargamos las urls del array
for (url in urls) {
  download.file(url, destfile = basename(url))
}
#guardamos los nombres de los archivos descargados
myfiles <- list.files(path = home, pattern = "pdf", full.names = TRUE)
#convierte los pdf en txt
lapply(myfiles, function(i) system(paste('"/usr/bin/pdftotext"', paste0('"', i, '"')), wait = FALSE) )

#leemos los txt
myfiles <- list.files(path = home, pattern = "txt", full.names = TRUE)

#Lo corpuamos
opinions <- Corpus(URISource(myfiles))
#aplicar reglas
opinions.tdm <- TermDocumentMatrix(opinions, control = list(removePunctuation = TRUE,
                                                            stopwords = TRUE,
                                                            tolower = TRUE,
                                                            stemming = TRUE,
                                                            removeNumbers = TRUE,
                                                            bounds = list(global = c(3, Inf)))) 
#lo pasamos a una matriz
m <- as.matrix(opinions.tdm)
#ordenamos
v <- sort(rowSums(m),decreasing=TRUE)

d <- data.frame(word = names(v),freq=v)
#sacamos el dibuin
wordcloud(words = d$word, freq = d$freq, min.freq = 1,scale=c(1,.5),
          max.words=Inf, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

