library(shiny)
library(data.table)

# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
#library(datasets)
library(ggplot2)
library(plotly)
library(dplyr)
#/Users/Work_JGI/Documents/r_working_folder/Baseball_player_comp/
merged<-read.csv('clean_merged_data_mlb.csv', header = TRUE, sep = ",")
merged$X<-NULL
deviance = NULL
g <- NULL

Bat <- merged[(merged$AB > 180),] 
Bat <- Bat[complete.cases(Bat),]

player_age<- 31


shinyServer(function(input, output) {
  
  ui_player<-reactive({
    name_x<-merged%>%
      filter(Name %in%input$Name)
    return(name_x)
  })
  
  
  ui_b_year<-reactive({
    birth_year<-data.frame(birthYear = ui_player()$birthYear)
    birth_year<-birth_year%>%
      distinct(birthYear)
    return(birth_year)
  })
  
  ui_age<-reactive({
    name_x<-merged%>%
      filter(Name %in%input$Name)
    age<-data.frame(name_x[,c(23,1,34,24)])
    age<-age%>%
      filter(birthYear == as.integer(as.character(input$b_year)))
    age<-max(age[,3])
    return(age)
  })
  
  output$b_year<- renderUI({
    selectInput("b_year", "Pick Player Birth Year:", choices = ui_b_year())  
  })
  
  #   output$ui <- renderUI({
  #     selectInput("p_age", "Player Age:", choices = max(ui_age()[,3]))  
  #     #ui_age()[,3]
  #   })
  #   
  
  Bat1<- reactive({
    Bat[(Bat$birthYear == as.integer(as.character(input$b_year))),]
    Bat[(Bat$Age == as.integer(as.character(ui_age()))),]
  })
  
  #creates a filtered data based on input$Name
  s1 <- reactive({
    i<-1
    Bat1 <- Bat[(Bat$Age == (as.integer(as.character(ui_age())) - 2)),]
    row1 <- Bat1[Bat1$Name %in% (input$Name[i]),21]
    row2 <- Bat1[Bat1$Name %in% (input$Name[i]),10]
    row3 <- Bat1[Bat1$Name %in% (input$Name[i]),20]
    row4 <- Bat1[Bat1$Name %in% (input$Name[i]),22]
    row5 <- Bat1[Bat1$Name %in% (input$Name[i]),9]
    
    ###Computing the Deviance as explained in note 9 of Nate Silver CARMELO article
    #mn1 <- mean(Bat1$AVG)
    mn2 <- mean(Bat1$RBI)
    mn3 <- mean(Bat1$SLG)
    #mn4 <- mean(Bat1$WAR)
    mn5 <- mean(Bat1$HR)
    #sd1 <- sd(Bat1$AVG)
    sd2 <- sd(Bat1$RBI)
    sd3 <- sd(Bat1$SLG)
    #sd4 <- sd(Bat1$WAR)
    sd5 <- sd(Bat1$HR)
    #a <- (row1-mn1)/sd1
    #b <- (Bat1[,14]-mn1)/sd1
    #diff1 <- ((a-b)^2)
    c <- (row2-mn2)/sd2
    d <- (Bat1[,10]-mn2)/sd2
    diff2 <- ((c-d)^2)
    e <- (row3-mn3)/sd3
    f <- (Bat1[,20]-mn3)/sd3
    diff3 <- ((e-f)^2)
    #                   h <- (row4-mn4)/sd4
    #                   i <- (Bat1[,22]-mn4)/sd4
    #                   diff4 <- ((h-i)^2)
    j <- (row5-mn5)/sd5
    k <- (Bat1[,9]-mn5)/sd5
    diff5 <- ((j-k)^2)
    #deviance <- sqrt(diff5)
    deviance <- sqrt((((diff3))*1)+(((diff5))*1)+(((diff2))*1))
    
    ###Similarity Score calculation
    SS <- 100*((1.25-deviance)/1.25)
    
    ###assign a unique variable name to the output, combine the sim scores with the associated player id's
    p <- data.frame(Name=Bat1$Name)
    #g <- paste("ss_index_", Bat1[Bat1$playerID %in% (my.inputs[i]),1], sep="")
    #assign(g, cbind(SS,p))
    g <- cbind(SS,p)
    SS1 <- g
    ds <- data.table(SS1, key="SS")
    ds <- ds[(as.numeric(ds$SS) > 0),]
    dsp <- data.frame(p=matrix(ds$Name))
    hash.table <- data.frame(key=ds$Name,value = ds$SS)
    s1 <- filter(Bat1(), Name %in% dsp$p)
    s1 <- s1[,c(23,9,10,20)]
    s1$Weight <- hash.table[match(s1$Name, hash.table$key),2]
    return(s1)
  })
  
  #     
  #     
  #     ##Computing season 2 ***unweighted*** averages from similar players
  s2<- reactive({
    Bat1 <- Bat[(Bat$Age == (as.integer(as.character(ui_age())) - 1)),]
    i<-1
    row1 <- Bat1[Bat1$Name %in% (input$Name[i]),21]
    row2 <- Bat1[Bat1$Name %in% (input$Name[i]),10]
    row3 <- Bat1[Bat1$Name %in% (input$Name[i]),20]
    row4 <- Bat1[Bat1$Name %in% (input$Name[i]),22]
    row5 <- Bat1[Bat1$Name %in% (input$Name[i]),9]
    
    ###Computing the Deviance as explained in note 9 of Nate Silver CARMELO article
    #mn1 <- mean(Bat1$AVG)
    mn2 <- mean(Bat1$RBI)
    mn3 <- mean(Bat1$SLG)
    #mn4 <- mean(Bat1$WAR)
    mn5 <- mean(Bat1$HR)
    #sd1 <- sd(Bat1$AVG)
    sd2 <- sd(Bat1$RBI)
    sd3 <- sd(Bat1$SLG)
    #sd4 <- sd(Bat1$WAR)
    sd5 <- sd(Bat1$HR)
    #a <- (row1-mn1)/sd1
    #b <- (Bat1[,14]-mn1)/sd1
    #diff1 <- ((a-b)^2)
    c <- (row2-mn2)/sd2
    d <- (Bat1[,10]-mn2)/sd2
    diff2 <- ((c-d)^2)
    e <- (row3-mn3)/sd3
    f <- (Bat1[,20]-mn3)/sd3
    diff3 <- ((e-f)^2)
    #                   h <- (row4-mn4)/sd4
    #                   i <- (Bat1[,22]-mn4)/sd4
    #                   diff4 <- ((h-i)^2)
    j <- (row5-mn5)/sd5
    k <- (Bat1[,9]-mn5)/sd5
    diff5 <- ((j-k)^2)
    #deviance <- sqrt(diff5)
    deviance <- sqrt((((diff3))*1)+(((diff5))*1)+(((diff2))*1))
    
    ###Similarity Score calculation
    SS <- 100*((1.25-deviance)/1.25)
    
    ###assign a unique variable name to the output, combine the sim scores with the associated player id's
    p <- data.frame(Name=Bat1$Name)
    #g <- paste("ss_index_", Bat1[Bat1$playerID %in% (my.inputs[i]),1], sep="")
    #assign(g, cbind(SS,p))
    g <- cbind(SS,p)
    
    SS2 <- g
    ds <- data.table(SS2, key="SS")
    ds <- ds[(as.numeric(ds$SS) > 0),]
    dsp <- data.frame(p=matrix(ds$Name))
    hash.table <- data.frame(key=ds$Name,value = ds$SS)
    s2 <- filter(Bat1(), Name %in% dsp$p)
    s2 <- s2[,c(23,9,10,20)]
    s2$Weight <- hash.table[match(s2$Name, hash.table$key),2]
    return(s2)
  })
  #     
  #     
  #     
  #     ##Computing season 3 ***unweighted*** averages from similar players
  s3 <- reactive({
    Bat1<-Bat[(Bat$Age == (as.integer(as.character(ui_age())))),]
    
    i<-1
    row1 <- Bat1[Bat1$Name %in% (input$Name[i]),21]
    row2 <- Bat1[Bat1$Name %in% (input$Name[i]),10]
    row3 <- Bat1[Bat1$Name %in% (input$Name[i]),20]
    row4 <- Bat1[Bat1$Name %in% (input$Name[i]),22]
    row5 <- Bat1[Bat1$Name %in% (input$Name[i]),9]
    
    ###Computing the Deviance as explained in note 9 of Nate Silver CARMELO article
    #mn1 <- mean(Bat1$AVG)
    mn2 <- mean(Bat1$RBI)
    mn3 <- mean(Bat1$SLG)
    #mn4 <- mean(Bat1$WAR)
    mn5 <- mean(Bat1$HR)
    #sd1 <- sd(Bat1$AVG)
    sd2 <- sd(Bat1$RBI)
    sd3 <- sd(Bat1$SLG)
    #sd4 <- sd(Bat1$WAR)
    sd5 <- sd(Bat1$HR)
    #a <- (row1-mn1)/sd1
    #b <- (Bat1[,14]-mn1)/sd1
    #diff1 <- ((a-b)^2)
    c <- (row2-mn2)/sd2
    d <- (Bat1[,10]-mn2)/sd2
    diff2 <- ((c-d)^2)
    e <- (row3-mn3)/sd3
    f <- (Bat1[,20]-mn3)/sd3
    diff3 <- ((e-f)^2)
    #                   h <- (row4-mn4)/sd4
    #                   i <- (Bat1[,22]-mn4)/sd4
    #                   diff4 <- ((h-i)^2)
    j <- (row5-mn5)/sd5
    k <- (Bat1[,9]-mn5)/sd5
    diff5 <- ((j-k)^2)
    #deviance <- sqrt(diff5)
    deviance <- sqrt((((diff3))*1)+(((diff5))*1)+(((diff2))*1))
    
    ###Similarity Score calculation
    SS <- 100*((1.25-deviance)/1.25)
    
    ###assign a unique variable name to the output, combine the sim scores with the associated player id's
    p <- data.frame(Name=Bat1$Name)
    #g <- paste("ss_index_", Bat1[Bat1$playerID %in% (my.inputs[i]),1], sep="")
    #assign(g, cbind(SS,p))
    g <- cbind(SS,p)
    SS3 <- g
    ds <- data.table(SS3, key="SS")
    ds <- ds[(as.numeric(ds$SS) > 0),]
    dsp <- data.frame(p=matrix(ds$Name))
    hash.table <- data.frame(key=ds$Name,value = ds$SS)
    s3 <- filter(Bat1(), Name %in% dsp$p)
    s3 <- s3[,c(23,9,10,20)]
    s3$Weight <- hash.table[match(s3$Name, hash.table$key),2]
    return(s3)
  })
  #     
  #     ##merge the three seasons and create the aggregated weights
  d1 <- reactive({rbind(s1(),s2())})
  d2 <- reactive({rbind(d1(),s3())})
  
  #output table showing result
  d3 <- reactive({
    d2<- d2()[,c(1,5)]
    d2$Weight <- as.integer(as.character(d2$Weight))
    d2<-d2%>%
      group_by(Name)%>%
      summarise_each(funs(sum))%>%
      arrange(desc(Weight))
    d2$Weight <- as.integer(as.character(d2$Weight))
    d2 <- head(d2,6)
    return(d2)
  })
  
  #used for projection
  d4 <- reactive({
    d2<- d2()[,c(1,5)]
    d2$Weight <- as.integer(as.character(d2$Weight))
    d2<-d2%>%
      group_by(Name)%>%
      summarise_each(funs(sum))%>%
      arrange(desc(Weight))
    d2$Weight <- as.integer(as.character(d2$Weight))
    
    return(d2)
  })
  
  player_plot1<-reactive({
    d2<- d2()[,c(1,5)]
    d2$Weight <- as.integer(as.character(d2$Weight))
    d2<-d2%>%
      group_by(Name)%>%
      summarise(Weight = sum(Weight))%>%
      arrange(desc(Weight))
    d2 <- head(d2,6)
    
    p_name<-data.frame(lapply(d2$Name[1], as.character), stringsAsFactors = FALSE)
    return(p_name)
  })
  
  player_proj<-reactive({
    ##Subset dataframe by similiar players and future seasons played
    d4<-d4()
    BBdata35 <- Bat[(Bat$Age > as.integer(as.character(ui_age()))),c(23,9,10,20,34)]
    Bat2 <- data.frame(Bat1()[(Bat1()$Name == input$Name),c(23,9,10,20,34)])
    Names <- as.vector(d4$Name) #create name list
    #filter by names list
    SimP <- BBdata35%>%
      filter(Name %in% d4$Name)
    #order names list
    SimP <- SimP[order(SimP$Name),]
    #function to do something???
    chg <- function(x) x-lag(x)
    #create new 
    SimP$HRch <- chg(SimP$HR)
    SimP$RBIch <- chg(SimP$RBI)
    SimP$SLGch <- chg(SimP$SLG)
    
    
    ##First year average changes
    SimP1 <- SimP[(SimP$Age %in% (min(SimP$Age)+1)),]
    #         SimP1<-SimP1%>%
    #               filter(Name != input$Name)
    SimP1<- left_join(SimP1, d4, by = "Name", copy = TRUE)
    SimP1<-na.omit(SimP1)
    
    #SimP1$weight <- d4[match(SimP1$Name, d4$Weight),2]
    #SimP1 <- SimP1[complete.cases(SimP1),]
    SimP1<-SimP1%>%
      arrange(desc(Weight))
    SimP1$Year1_HR<-SimP1$Weight*SimP1$HRch
    SimP1$Year1_RBI<-SimP1$RBIch*SimP1$Weight
    SimP1$Year1_SLG <- SimP1$SLGch*SimP1$Weight
    arcd<-SimP1%>%
      summarise(sum_HR = sum(Year1_HR), sum_RBI = sum(Year1_RBI), sum_SLG = sum(Year1_SLG), sum_Weight = sum(Weight))
    year_1<-data.frame(Year = "Year_1",  HR = (arcd$sum_HR/arcd$sum_Weight), RBI = (arcd$sum_RBI/arcd$sum_Weight), SLG = (arcd$sum_SLG/arcd$sum_Weight), Age = as.integer(as.character(ui_age()))+1)
    
    
    ##Second Year average changes
    SimP2 <- SimP[(SimP$Age %in% (min(SimP$Age)+2)),]
    #         SimP2<-SimP2%>%
    #           filter(Name != input$Name)
    SimP2<- left_join(SimP2, d4, by = "Name", copy = TRUE)
    SimP2<-na.omit(SimP2)
    SimP2$Year2_HR<-SimP2$Weight*SimP2$HRch
    SimP2$Year2_RBI<-SimP2$RBIch*SimP2$Weight
    SimP2$Year2_SLG <- SimP2$SLGch*SimP2$Weight
    arcd<-SimP2%>%
      summarise(sum_HR = sum(Year2_HR), sum_RBI = sum(Year2_RBI), sum_SLG = sum(Year2_SLG), sum_Weight = sum(Weight))
    year_2<-data.frame(Year = "Year_2",  HR = (arcd$sum_HR/arcd$sum_Weight), RBI = (arcd$sum_RBI/arcd$sum_Weight), SLG = (arcd$sum_SLG/arcd$sum_Weight), Age = as.integer(as.character(ui_age()))+2)
    
    
    ##Third Year average changes
    SimP3 <- SimP[(SimP$Age %in% (min(SimP$Age)+3)),]
    #         SimP3<-SimP3%>%
    #           filter(Name != input$Name)
    SimP3<- left_join(SimP3, d4, by = "Name", copy = TRUE)
    SimP3<-na.omit(SimP3)
    SimP3$Year3_HR<-SimP3$Weight*SimP3$HRch
    SimP3$Year3_RBI<-SimP3$RBIch*SimP3$Weight
    SimP3$Year3_SLG <- SimP3$SLGch*SimP3$Weight
    arcd<-SimP3%>%
      summarise(sum_HR = sum(Year3_HR), sum_RBI = sum(Year3_RBI), sum_SLG = sum(Year3_SLG), sum_Weight = sum(Weight))
    year_3<-data.frame(Year = "Year_3",  HR = (arcd$sum_HR/arcd$sum_Weight), RBI = (arcd$sum_RBI/arcd$sum_Weight), SLG = (arcd$sum_SLG/arcd$sum_Weight), Age = as.integer(as.character(ui_age()))+3) 
    
    
    Proj_1<-data.frame(Name = input$Name, HR = Bat2$HR + year_1$HR, RBI = Bat2$RBI + year_1$RBI, SLG = Bat2$SLG + year_1$SLG, Age = as.integer(as.character(ui_age()))+1)
    Proj_2<-data.frame(Name = input$Name, HR = Proj_1$HR + year_2$HR, RBI = Proj_1$RBI + year_2$RBI, SLG = Proj_1$SLG + year_2$SLG, Age = as.integer(as.character(ui_age()))+2)
    Proj_3<-data.frame(Name = input$Name, HR = Proj_2$HR + year_3$HR, RBI = Proj_2$RBI + year_3$RBI, SLG = Proj_2$SLG + year_3$SLG, Age = as.integer(as.character(ui_age()))+3)
    Projection<-rbind(Proj_1, Proj_2) 
    Projection<-rbind(Projection, Proj_3)  
    
    
    return(Projection)
  })
  
  c_proj<-reactive({
    name_x<-merged%>%
      filter(Name %in%input$Name)%>%
      filter(birthYear == as.integer(as.character(input$b_year)))%>%
      filter(Age <= as.integer(as.character(ui_age())))%>%
      arrange(Age)
    name_x<-name_x[,c(23,9,10,20,34)]
    Projection<-player_proj()
    c_projection<-rbind(name_x,Projection)
    return(c_projection)
  })
  
  first_career_avg<-reactive({
    name_x<-merged%>%
      filter(Name %in%input$Name)%>%
      filter(birthYear == as.integer(as.character(input$b_year)))%>%
      filter(Age <= as.integer(as.character(ui_age())))%>%
      arrange(Age)
    name_x<-name_x[,c(23,9,10,20)]%>%
      group_by(Name)%>%
      summarise_each(funs(mean))
    
    return(name_x)
  })
  
  
  my.p1<-reactive({
    a<-merged%>%
      filter(Name %in% player_plot1())%>%
      filter(birthYear %in% input$b_year)%>%
      arrange((Age))
    
  })
  ###################################  ###################################  
  ########second player projection
  ###################################  ###################################  
  
  sec_player<-reactive({
    name_y<-merged%>%
      filter(Name %in% input$sec_Name)
    return(name_y)
  })
  
  sec_b_year<-reactive({
    birth_year<-data.frame(birthYear = sec_player()$birthYear)
    birth_year<-birth_year%>%
      distinct(birthYear)
    return(birth_year)
  })
  
  output$s_b_year<- renderUI({
    selectInput("s_b_year", "Pick Player Birth Year:", choices = sec_b_year())  
  })
  
  output$sec_ui <- renderUI({
    selectInput("sec_p_age", "Player Age:", choices = max(sec_age()[,3]))  
  })
  
  
  sec_age<-reactive({
    name_x<-merged%>%
      filter(Name %in%input$sec_Name)
    age<-data.frame(name_x[,c(23,1,34,24)])
    age<-age%>%
      filter(birthYear == as.integer(as.character(input$s_b_year)))
    age<-max(age[,3])
    return(age)
  })
  
  sec_Bat1<- reactive({
    Bat[(Bat$birthYear == as.integer(as.character(input$sec_Name))),]
    Bat[(Bat$Age == as.integer(as.character(sec_age()))),]
  })
  
  sec_s1 <- reactive({
    i<-1
    Bat1 <- Bat[(Bat$Age == (as.integer(as.character(sec_age())) - 2)),]
    row1 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),21]
    row2 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),10]
    row3 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),20]
    row4 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),22]
    row5 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),9]
    
    ###Computing the Deviance as explained in note 9 of Nate Silver CARMELO article
    #mn1 <- mean(Bat1$AVG)
    mn2 <- mean(Bat1$RBI)
    mn3 <- mean(Bat1$SLG)
    #mn4 <- mean(Bat1$WAR)
    mn5 <- mean(Bat1$HR)
    #sd1 <- sd(Bat1$AVG)
    sd2 <- sd(Bat1$RBI)
    sd3 <- sd(Bat1$SLG)
    #sd4 <- sd(Bat1$WAR)
    sd5 <- sd(Bat1$HR)
    #a <- (row1-mn1)/sd1
    #b <- (Bat1[,14]-mn1)/sd1
    #diff1 <- ((a-b)^2)
    c <- (row2-mn2)/sd2
    d <- (Bat1[,10]-mn2)/sd2
    diff2 <- ((c-d)^2)
    e <- (row3-mn3)/sd3
    f <- (Bat1[,20]-mn3)/sd3
    diff3 <- ((e-f)^2)
    #                   h <- (row4-mn4)/sd4
    #                   i <- (Bat1[,22]-mn4)/sd4
    #                   diff4 <- ((h-i)^2)
    j <- (row5-mn5)/sd5
    k <- (Bat1[,9]-mn5)/sd5
    diff5 <- ((j-k)^2)
    #deviance <- sqrt(diff5)
    deviance <- sqrt((((diff3))*1)+(((diff5))*1)+(((diff2))*1))
    
    ###Similarity Score calculation
    SS <- 100*((1.25-deviance)/1.25)
    
    ###assign a unique variable name to the output, combine the sim scores with the associated player id's
    p <- data.frame(Name=Bat1$Name)
    #g <- paste("ss_index_", Bat1[Bat1$playerID %in% (my.inputs[i]),1], sep="")
    #assign(g, cbind(SS,p))
    g <- cbind(SS,p)
    SS1 <- g
    ds <- data.table(SS1, key="SS")
    ds <- ds[(as.numeric(ds$SS) > 0),]
    dsp <- data.frame(p=matrix(ds$Name))
    hash.table <- data.frame(key=ds$Name,value = ds$SS)
    s1 <- filter(sec_Bat1(), Name %in% dsp$p)
    s1 <- s1[,c(23,9,10,20)]
    s1$Weight <- hash.table[match(s1$Name, hash.table$key),2]
    return(s1)
  })
  
  sec_s2<- reactive({
    Bat1 <- Bat[(Bat$Age == (as.integer(as.character(sec_age())) - 1)),]
    i<-1
    row1 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),21]
    row2 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),10]
    row3 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),20]
    row4 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),22]
    row5 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),9]
    
    ###Computing the Deviance as explained in note 9 of Nate Silver CARMELO article
    #mn1 <- mean(Bat1$AVG)
    mn2 <- mean(Bat1$RBI)
    mn3 <- mean(Bat1$SLG)
    #mn4 <- mean(Bat1$WAR)
    mn5 <- mean(Bat1$HR)
    #sd1 <- sd(Bat1$AVG)
    sd2 <- sd(Bat1$RBI)
    sd3 <- sd(Bat1$SLG)
    #sd4 <- sd(Bat1$WAR)
    sd5 <- sd(Bat1$HR)
    #a <- (row1-mn1)/sd1
    #b <- (Bat1[,14]-mn1)/sd1
    #diff1 <- ((a-b)^2)
    c <- (row2-mn2)/sd2
    d <- (Bat1[,10]-mn2)/sd2
    diff2 <- ((c-d)^2)
    e <- (row3-mn3)/sd3
    f <- (Bat1[,20]-mn3)/sd3
    diff3 <- ((e-f)^2)
    #                   h <- (row4-mn4)/sd4
    #                   i <- (Bat1[,22]-mn4)/sd4
    #                   diff4 <- ((h-i)^2)
    j <- (row5-mn5)/sd5
    k <- (Bat1[,9]-mn5)/sd5
    diff5 <- ((j-k)^2)
    #deviance <- sqrt(diff5)
    deviance <- sqrt((((diff3))*1)+(((diff5))*1)+(((diff2))*1))
    
    ###Similarity Score calculation
    SS <- 100*((1.25-deviance)/1.25)
    
    ###assign a unique variable name to the output, combine the sim scores with the associated player id's
    p <- data.frame(Name=Bat1$Name)
    #g <- paste("ss_index_", Bat1[Bat1$playerID %in% (my.inputs[i]),1], sep="")
    #assign(g, cbind(SS,p))
    g <- cbind(SS,p)
    
    SS2 <- g
    ds <- data.table(SS2, key="SS")
    ds <- ds[(as.numeric(ds$SS) > 0),]
    dsp <- data.frame(p=matrix(ds$Name))
    hash.table <- data.frame(key=ds$Name,value = ds$SS)
    s2 <- filter(sec_Bat1(), Name %in% dsp$p)
    s2 <- s2[,c(23,9,10,20)]
    s2$Weight <- hash.table[match(s2$Name, hash.table$key),2]
    return(s2)
  })
  
  sec_s3 <- reactive({
    Bat1<-Bat[(Bat$Age == (as.integer(as.character(sec_age())))),]
    
    i<-1
    row1 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),21]
    row2 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),10]
    row3 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),20]
    row4 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),22]
    row5 <- Bat1[Bat1$Name %in% (input$sec_Name[i]),9]
    
    ###Computing the Deviance as explained in note 9 of Nate Silver CARMELO article
    #mn1 <- mean(Bat1$AVG)
    mn2 <- mean(Bat1$RBI)
    mn3 <- mean(Bat1$SLG)
    #mn4 <- mean(Bat1$WAR)
    mn5 <- mean(Bat1$HR)
    #sd1 <- sd(Bat1$AVG)
    sd2 <- sd(Bat1$RBI)
    sd3 <- sd(Bat1$SLG)
    #sd4 <- sd(Bat1$WAR)
    sd5 <- sd(Bat1$HR)
    #a <- (row1-mn1)/sd1
    #b <- (Bat1[,14]-mn1)/sd1
    #diff1 <- ((a-b)^2)
    c <- (row2-mn2)/sd2
    d <- (Bat1[,10]-mn2)/sd2
    diff2 <- ((c-d)^2)
    e <- (row3-mn3)/sd3
    f <- (Bat1[,20]-mn3)/sd3
    diff3 <- ((e-f)^2)
    #                   h <- (row4-mn4)/sd4
    #                   i <- (Bat1[,22]-mn4)/sd4
    #                   diff4 <- ((h-i)^2)
    j <- (row5-mn5)/sd5
    k <- (Bat1[,9]-mn5)/sd5
    diff5 <- ((j-k)^2)
    #deviance <- sqrt(diff5)
    deviance <- sqrt((((diff3))*1)+(((diff5))*1)+(((diff2))*1))
    
    ###Similarity Score calculation
    SS <- 100*((1.25-deviance)/1.25)
    
    ###assign a unique variable name to the output, combine the sim scores with the associated player id's
    p <- data.frame(Name=Bat1$Name)
    #g <- paste("ss_index_", Bat1[Bat1$playerID %in% (my.inputs[i]),1], sep="")
    #assign(g, cbind(SS,p))
    g <- cbind(SS,p)
    SS3 <- g
    ds <- data.table(SS3, key="SS")
    ds <- ds[(as.numeric(ds$SS) > 0),]
    dsp <- data.frame(p=matrix(ds$Name))
    hash.table <- data.frame(key=ds$Name,value = ds$SS)
    s3 <- filter(sec_Bat1(), Name %in% dsp$p)
    s3 <- s3[,c(23,9,10,20)]
    s3$Weight <- hash.table[match(s3$Name, hash.table$key),2]
    return(s3)
  })
  
  sec_d1 <- reactive({rbind(sec_s1(),sec_s2())})
  sec_d2 <- reactive({rbind(sec_d1(),sec_s3())})
  
  sec_d3 <- reactive({
    d2<- sec_d2()[,c(1,5)]
    d2$Weight <- as.integer(as.character(d2$Weight))
    d2<-d2%>%
      group_by(Name)%>%
      summarise_each(funs(sum))%>%
      arrange(desc(Weight))
    d2$Weight <- as.integer(as.character(d2$Weight))
    d2 <- head(d2,6)
    return(d2)
  })
  
  sec_d4 <- reactive({
    d2<- sec_d2()[,c(1,5)]
    d2$Weight <- as.integer(as.character(d2$Weight))
    d2<-d2%>%
      group_by(Name)%>%
      summarise_each(funs(sum))%>%
      arrange(desc(Weight))
    d2$Weight <- as.integer(as.character(d2$Weight))
    
    return(d2)
  })
  
  sec_player_proj<-reactive({
    ##Subset dataframe by similiar players and future seasons played
    d4<-sec_d4()
    BBdata35 <- Bat[(Bat$Age > as.integer(as.character(sec_age()))),c(23,9,10,20,34)]
    Bat2 <- data.frame(sec_Bat1()[(sec_Bat1()$Name == input$sec_Name),c(23,9,10,20,34)])
    Names <- as.vector(d4$Name) #create name list
    #filter by names list
    SimP <- BBdata35%>%
      filter(Name %in% d4$Name)
    #order names list
    SimP <- SimP[order(SimP$Name),]
    #function to do something???
    chg <- function(x) x-lag(x)
    #create new 
    SimP$HRch <- chg(SimP$HR)
    SimP$RBIch <- chg(SimP$RBI)
    SimP$SLGch <- chg(SimP$SLG)
    
    
    ##First year average changes
    SimP1 <- SimP[(SimP$Age %in% (min(SimP$Age)+1)),]
    #         SimP1<-SimP1%>%
    #               filter(Name != input$Name)
    SimP1<- left_join(SimP1, d4, by = "Name", copy = TRUE)
    SimP1<-na.omit(SimP1)
    
    #SimP1$weight <- d4[match(SimP1$Name, d4$Weight),2]
    #SimP1 <- SimP1[complete.cases(SimP1),]
    SimP1<-SimP1%>%
      arrange(desc(Weight))
    SimP1$Year1_HR<-SimP1$Weight*SimP1$HRch
    SimP1$Year1_RBI<-SimP1$RBIch*SimP1$Weight
    SimP1$Year1_SLG <- SimP1$SLGch*SimP1$Weight
    arcd<-SimP1%>%
      summarise(sum_HR = sum(Year1_HR), sum_RBI = sum(Year1_RBI), sum_SLG = sum(Year1_SLG), sum_Weight = sum(Weight))
    year_1<-data.frame(Year = "Year_1",  HR = (arcd$sum_HR/arcd$sum_Weight), RBI = (arcd$sum_RBI/arcd$sum_Weight), SLG = (arcd$sum_SLG/arcd$sum_Weight), Age = as.integer(as.character(sec_age()))+1)
    
    
    ##Second Year average changes
    SimP2 <- SimP[(SimP$Age %in% (min(SimP$Age)+2)),]
    #         SimP2<-SimP2%>%
    #           filter(Name != input$Name)
    SimP2<- left_join(SimP2, d4, by = "Name", copy = TRUE)
    SimP2<-na.omit(SimP2)
    SimP2$Year2_HR<-SimP2$Weight*SimP2$HRch
    SimP2$Year2_RBI<-SimP2$RBIch*SimP2$Weight
    SimP2$Year2_SLG <- SimP2$SLGch*SimP2$Weight
    arcd<-SimP2%>%
      summarise(sum_HR = sum(Year2_HR), sum_RBI = sum(Year2_RBI), sum_SLG = sum(Year2_SLG), sum_Weight = sum(Weight))
    year_2<-data.frame(Year = "Year_2",  HR = (arcd$sum_HR/arcd$sum_Weight), RBI = (arcd$sum_RBI/arcd$sum_Weight), SLG = (arcd$sum_SLG/arcd$sum_Weight), Age = as.integer(as.character(sec_age()))+2)
    
    
    ##Third Year average changes
    SimP3 <- SimP[(SimP$Age %in% (min(SimP$Age)+3)),]
    #         SimP3<-SimP3%>%
    #           filter(Name != input$Name)
    SimP3<- left_join(SimP3, d4, by = "Name", copy = TRUE)
    SimP3<-na.omit(SimP3)
    SimP3$Year3_HR<-SimP3$Weight*SimP3$HRch
    SimP3$Year3_RBI<-SimP3$RBIch*SimP3$Weight
    SimP3$Year3_SLG <- SimP3$SLGch*SimP3$Weight
    arcd<-SimP3%>%
      summarise(sum_HR = sum(Year3_HR), sum_RBI = sum(Year3_RBI), sum_SLG = sum(Year3_SLG), sum_Weight = sum(Weight))
    year_3<-data.frame(Year = "Year_3",  HR = (arcd$sum_HR/arcd$sum_Weight), RBI = (arcd$sum_RBI/arcd$sum_Weight), SLG = (arcd$sum_SLG/arcd$sum_Weight), Age = as.integer(as.character(sec_age()))+3) 
    
    
    Proj_1<-data.frame(Name = input$sec_Name, HR = Bat2$HR + year_1$HR, RBI = Bat2$RBI + year_1$RBI, SLG = Bat2$SLG + year_1$SLG, Age = as.integer(as.character(sec_age()))+1)
    Proj_2<-data.frame(Name = input$sec_Name, HR = Proj_1$HR + year_2$HR, RBI = Proj_1$RBI + year_2$RBI, SLG = Proj_1$SLG + year_2$SLG, Age = as.integer(as.character(sec_age()))+2)
    Proj_3<-data.frame(Name = input$sec_Name, HR = Proj_2$HR + year_3$HR, RBI = Proj_2$RBI + year_3$RBI, SLG = Proj_2$SLG + year_3$SLG, Age = as.integer(as.character(sec_age()))+3)
    Projection<-rbind(Proj_1, Proj_2) 
    Projection<-rbind(Projection, Proj_3)  
    
    
    return(Projection)
  })
  
  sec_c_proj<-reactive({
    name_x<-merged%>%
      filter(Name %in%input$sec_Name)%>%
      filter(birthYear == as.integer(as.character(input$s_b_year)))%>%
      filter(Age <= as.integer(as.character(sec_age())))%>%
      arrange(Age)
    name_x<-name_x[,c(23,9,10,20,34)]
    Projection<-sec_player_proj()
    c_projection<-rbind(name_x,Projection)
    return(c_projection)
  })
  
  sec_career_avg<-reactive({
    name_x<-merged%>%
      filter(Name %in%input$sec_Name)%>%
      filter(birthYear == as.integer(as.character(input$s_b_year)))%>%
      filter(Age <= as.integer(as.character(sec_age())))%>%
      arrange(Age)
    name_x<-name_x[,c(23,9,10,20)]
    name_x<-name_x%>%
      group_by(Name)%>%
      summarise_each(funs(mean))
    return(name_x)
  })
  
  proj_comb<-reactive({
    first<-player_proj()
    year<-2016
    first$yearID<-c(year+1,year+2,year+3)
    second<-sec_player_proj()
    second$yearID<-c(year+1,year+2,year+3)
    comb<-rbind(first,second)
    return(comb)
  })
  
  proj_w_year_first<-reactive({
    first<-player_proj()
    year<-2016
    first$yearID<-c(year+1,year+2,year+3)
    return(first)
  })
  
  proj_w_year_sec<-reactive({
    second<-sec_player_proj()
    year<-2016
    second$yearID<-c(year+1,year+2,year+3)
    return(second)
  })
  
  output$f_proj<- renderTable({
    #d4()
    sec_career_avg()
  })
  
  output$sec_proj<- renderTable({
    first_career_avg()
  })
  
  output$f_proj_avg<- renderTable({
    #d4()
    sec<-sec_player_proj()[,c(1,2,3,4)]%>%
      group_by(Name)%>%
      summarise_each(funs(mean))
    return(sec)
  })
  
  output$sec_proj_avg<- renderTable({
    first<-player_proj()[,c(1,2,3,4)]%>%
      group_by(Name)%>%
      summarise_each(funs(mean))
    return(first)
  })
  
  output$f_com_player<-renderTable({
    return(d3())
  })
  
  output$sec_com_player<-renderTable({
    return(sec_d3())
  })
  
  
  output$plotmain<- renderPlotly({
    
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right"
    ) 
    
    p1<-plot_ly(c_proj(), x = Age, y = SLG, mode = 'smooth', name = 'Slugging %age')%>%
      add_trace(x = Age, y = HR, name = "HR", yaxis = "y2") %>% #second y-axis
      layout(title = paste(input$Name,"'s Projected HR and SLG From Age ",as.integer(as.character(ui_age())), " - ", as.integer(as.character(ui_age()))+3 ),yaxis2 = ay)%>%
      add_trace(x = c(as.integer(as.character(ui_age())), as.integer(as.character(ui_age()))+3), y= c(max(SLG), max(SLG)),fill = "tozeroy", mode = "lines", name = paste(input$Name, "'s Porjected SLG %age", sep = ""), showlegend = FALSE)
    
    layout(p1)
  })
  output$plotmain_2<- renderPlotly({
    
    
    ay <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right"
    ) 
    
    p1<-plot_ly(sec_c_proj(), x = Age, y = SLG, mode = 'smooth', name = 'Slugging %age')%>%
      add_trace(x = Age, y = HR, name = "HR", yaxis = "y2") %>% #second y-axis
      layout(title = paste(input$sec_Name,"'s Projected HR and SLG From Age ",as.integer(as.character(sec_age())), " - ", as.integer(as.character(sec_age()))+3 ),yaxis2 = ay)%>%
      add_trace(x = c(as.integer(as.character(sec_age())), as.integer(as.character(sec_age()))+3), y= c(max(SLG), max(SLG)),fill = "tozeroy", mode = "lines", name = paste(input$sec_Name, "'s Porjected SLG %age", sep = ""), showlegend = FALSE)
    
    layout(p1)
  })
  output$comparison_HR<- renderPlotly({
    x<-plot_ly(proj_comb(), x =proj_comb()$yearID, y= proj_comb()$HR, type = 'bar', color = proj_comb()$Name)%>%
      layout(title = 'Projected HR', xaxis = list(title='Year'), yaxis = list(title= 'HR'), legend = list(x = 0, y = 0))
    
    
    #       add_trace(x =proj_comb()$year, y= proj_comb()$SLG, mode = 'smooth',yaxis = "y2", color = proj_comb()$Name)%>%
    #       layout(title = "Double Y Axis", yaxis2 = ay)
    layout(x)
  })
  output$comparison_SLG<- renderPlotly({
    x<-plot_ly(proj_comb(), x =proj_comb()$yearID, y= proj_comb()$SLG, type = 'bar', color = proj_comb()$Name)%>%
      layout(title = 'Projected SLG', xaxis = list(title='Year'), yaxis = list(title= 'SLG Percentage'), legend = list(x = 0, y = 0))
    
    #       add_trace(x =proj_comb()$year, y= proj_comb()$SLG, mode = 'smooth',yaxis = "y2", color = proj_comb()$Name)%>%
    #       layout(title = "Double Y Axis", yaxis2 = ay)
    layout(x)
  })
  output$comparison_RBI<- renderPlotly({
    x<-plot_ly(proj_comb(), x =proj_comb()$yearID, y= proj_comb()$RBI, type = 'bar', color = proj_comb()$Name)%>%
      layout(title = 'Projected RBI', xaxis = list(title='Year'), yaxis = list(title= 'RBI'), legend = list(x = 0, y = 0))
    
    #       add_trace(x =proj_comb()$year, y= proj_comb()$SLG, mode = 'smooth',yaxis = "y2", color = proj_comb()$Name)%>%
    #       layout(title = "Double Y Axis", yaxis2 = ay)
    
    layout(x)
  })
})

#})
#   # Fill in the spot we created for a plot
#   output$mlbPlot <- renderPlotly({
#     #create dual axis for plotly
#     ay <- list(
#       tickfont = list(color = "red"),
#       overlaying = "y",
#       side = "right"
#     )
#     
#     # Render a dual axis plot using plotly
#     p<-plot_ly(selectedData(), x = Age, y = salary.w..inflation, mode = 'smooth', name = 'Salary w inflation')%>%
#       add_trace(x = Age, y = SLG, name = "SLG", yaxis = "y2") %>% #second y-axis
#       layout(title = paste(input$Name, min(year),'-', max(year)), yaxis2 = ay)#custom layout
#     
#     print(p)
#   })