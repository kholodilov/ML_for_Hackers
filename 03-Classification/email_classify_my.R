library(tm)
library(ggplot2)

spam.path <- "data/spam/"
spam2.path <- "data/spam_2/"
easyham.path <- "data/easy_ham/"
easyham2.path <- "data/easy_ham_2/"
hardham.path <- "data/hard_ham/"
hardham2.path <- "data/hard_ham_2/"

get.msg <- function(path) {
  con <- file(path, open="rt", encoding="latin1")
  text <- readLines(con)
  # The message always begins after the first full line break 
  msg <- text[seq(which(text=="")[1]+1,length(text),1)]
  close(con)
  return(paste(msg, collapse="\n"))
}

get.tdm <- function(doc.vec) {
  doc.corpus <- Corpus(VectorSource(doc.vec))
  control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE,
                  minDocFreq=2)
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

get.training.df <- function(msg.vector) {
  term.document.matrix <- as.matrix(get.tdm(msg.vector))
  counts <- rowSums(term.document.matrix)
  training.df <- data.frame(cbind(names(counts), as.numeric(counts)), stringsAsFactors=FALSE)
  names(training.df) <- c("term","frequency")
  training.df$frequency <- as.numeric(training.df$frequency)
  training.df$occurrence <- sapply(1:nrow(term.document.matrix),
                                   function(i) {length(which(term.document.matrix[i,] > 0))/ncol(term.document.matrix)})
  training.df$density <- training.df$frequency/sum(training.df$frequency)
  return(training.df)
}

classify.email <- function(path, training.df, prior=0.5, c=1e-6) {
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  # Find intersections of words
  msg.match <- intersect(names(msg.freq), training.df$term)
  if(length(msg.match) < 1) {
    return(prior*c^(length(msg.freq)))
  }
  else {
    match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
    return(prior * prod(match.probs) * c^(length(msg.freq)-length(msg.match)))
  }
}

spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs!="cmds")]
all.spam <- sapply(spam.docs, function(p) get.msg(file.path(spam.path,p)))
#head(all.spam)

spam.df <- get.training.df(all.spam)
#head(spam.df[with(spam.df, order(-occurrence)),])
#head(spam.df[with(spam.df, order(-frequency)),])

easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs!="cmds")]
all.easyham <- sapply(easyham.docs[1:length(spam.docs)],
                      function(p) get.msg(file.path(easyham.path,p)))
#head(all.easyham)

easyham.df <- get.training.df(all.easyham)
#head(easyham.df[with(easyham.df, order(-occurrence)),])
#head(easyham.df[with(easyham.df, order(-frequency)),])

hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs != "cmds")]
hardham.spamtest <- sapply(hardham.docs,
   function(p) classify.email(paste(hardham.path, p, sep=""), training.df=spam.df))
hardham.hamtest <- sapply(hardham.docs,
    function(p) classify.email(paste(hardham.path, p, sep=""), training.df=easyham.df))
hardham.res <- ifelse(hardham.spamtest > hardham.hamtest, TRUE, FALSE)
summary(hardham.res)
