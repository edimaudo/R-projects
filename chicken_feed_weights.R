#use in built dataset
data("chickwts")
plot(chickwts$feed)

feeds <- table(chickwts$feed)
barplot(feeds[order(feeds,decreasing = TRUE)])

#par - parameters
par(oma = c(1,1,1,1)) # outside margin
par(mar = c(4,5,2,1)) #plot margins
barplot(feeds[order(feeds)], 
        horiz  = TRUE,
        las    = 1,  # las gives orientation of axis labels
        col    = c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
        border = NA,  # No borders on bars
        main   = "Frequencies of Different Feeds\nin chickwts Dataset",  # \n = line break
        xlab   = "Number of Chicks")
