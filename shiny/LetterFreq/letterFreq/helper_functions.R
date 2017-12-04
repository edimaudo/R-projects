#helper functions

library(ggplot2)

sanitize <- function(str){
  #input: raw text string
  #output: relative frequencies
  
  str <- substr(str,1,30000)
  
  newstr <- gsub("[^[:alpha:]]", "", str)
  newstr <- tolower(newstr)
  chars <- strsplit(newstr, NULL)[[1]]
  freqs <- summary(factor(chars))
  rfreqs <- freqs / sum(freqs)
  
  return(freqs)
}

fill_freqs <- function(freqs) {
  #input: frequencies (not necessarily for all 26 letters)
  #output: frequencies for all 26 letters
  
  #make 0-count vector
  alphabet <- rep(1,26)
  names(alphabet) <- letters
  
  alphabet_filled <- sapply(1:26, function(x) {freqs[names(alphabet)[x]]})
  
  alphabet_filled[is.na(alphabet_filled)] <- 0
  
  names(alphabet_filled) <- letters
  alphabet_filled <- as.data.frame(alphabet_filled)
  names(alphabet_filled) <- "rfreqs"
  
  return(alphabet_filled)
}



fplot <- function(str,ref){
  #input: raw text, array with reference frequencies
  #output: plot
  freqs <- sanitize(str)
  rfreqs <- freqs/sum(freqs)
  
  alphabet_filled_freqs <- fill_freqs(freqs)
  alphabet_filled_rfreqs <- fill_freqs(rfreqs)
  
  alphabet_filled_new <- merge(alphabet_filled_freqs,alphabet_filled_rfreqs,by=0,all=TRUE)
  rownames(alphabet_filled_new) <- rownames(alphabet_filled_freqs)
  colnames(alphabet_filled_new) <- c("Row.names", "freqs", "rfreqs")
  
  if (ref[1] != "None") {
    alphabet_filled_ref <- fill_freqs(ref)
    
    #two-hist plot
    p <- ggplot(data=alphabet_filled_new, aes(x=seq(1:26),y=rfreqs)) + geom_bar(stat="identity",aes(fill=rfreqs)) + 
      scale_x_discrete(breaks = c(1:26), labels=letters) + 
      geom_bar(data=alphabet_filled_ref,stat="identity",color="salmon",alpha = 0) +
      theme(legend.position="none") +
      geom_text(aes(label=freqs,color=freqs),size=3,hjust=0,vjust=-0.5) + 
      labs(title="Relative Letter Frequency", x = "letter", y="relative frequency")
    
  } else {
    #one-hist plot
    p <- ggplot(data=alphabet_filled_new, aes(x=seq(1:26),y=rfreqs)) + geom_bar(stat="identity",aes(fill=rfreqs)) + 
      scale_x_discrete(breaks = c(1:26), labels=letters) +
      theme(legend.position="none") +
      geom_text(aes(label=freqs,color=freqs),size=3,hjust=0,vjust=-0.5) + 
      labs(title="Relative Letter Frequency", x = "letter", y="relative frequency")
  }
  
  print(p)
}