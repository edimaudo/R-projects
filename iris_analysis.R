# load the iris data

head(iris) # check some of the elements in the iris data
plot(iris) # product different views of iris data
plot(iris, col=iris$Species) # add color to plot

#plot Sepal width and length
attach(iris)
plot(x=Sepal.Width, y=Sepal.Length)
detach(iris)

# plot histogram of sepal width
hist(iris$Sepal.Width)

virginica <- subset(iris, Species=="virginica")
versicolor <- subset(iris, Species=="versicolor")
setosa <- subset(iris, Species=="setosa")
# plot distributions for each species
plot(density(virginica$Sepal.Width), col="blue")
lines(density(versicolor$Sepal.Width), col="red")
lines(density(setosa$Sepal.Width), col="green")
legend(2, 1.2, c("virginica", "versicolor", "setosa"), c("blue", "red", "green"))