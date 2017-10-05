A <- data.frame(Gender = c("F", "F", "M", "F", "B", "M", "M"), 
                Height = c(154, 167, 178, 145, 169, 183, 176))

#recoding
A[,1] <- ifelse(A[,1] == "M", 1, ifelse(A[,1] == "F", 2, 99))