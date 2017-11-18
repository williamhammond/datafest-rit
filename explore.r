# William Hammond
install_github("ggbiplot", "vqv")

library("reshape2")
library("dplyr")
library("devtools")
library("ggbiplot")
library("ggplot2")
library("rworldmap")
library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)

setwd('/home/william/DataFest')

dest.subset <- read.csv('dest.csv', sep = '\t',  stringsAsFactors=FALSE)

dest.subset <- sample_n(dest, 10000)
dest.subset <- dest.subset[complete.cases(dest.subset),]
#dest.subset["cluster"] <- array(pop.fit$cluster)

popularity <- subset(dest.subset, select = c(-srch_destination_id, 
                                             -srch_destination_name, 
                                             -srch_destination_type_id, 
                                             -srch_destination_longitude, 
                                             -srch_destination_latitude))

# K-Means
for (i in 1:dim(popularity)[2]) {
  popularity[, i] <- 10^popularity[, i]
  xs <- popularity[, i]
  min_x <- min(xs)
  max_x <- max(xs)
  popularity[, i] <- normalize(xs, min_x, max_x) * 100
}

wss <- (nrow(popularity)-1)*sum(apply(popularity, 2, var))
for (i in 2:30) wss[i] <- sum(kmeans(popularity,
                                     centers=i)$withinss)

plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Inter-Cluster Sum of Squares") 

pop.fit <- kmeans(popularity, 11)
popularity["cluster"] <- array(pop.fit$cluster)
dest.subset["cluster"] <- array(pop.fit$cluster)
pop.fit.agg <- aggregate(popularity,by=list(pop.fit$cluster),FUN=mean)

# Plot Geospatial Cluster
newmap <- getMap(resolution = "high")
jpeg(
  "world.jpeg",
  width     = 1600,
  height    = 900,
  units     = "px",
  pointsize = 12
)
plot(newmap, xlim = c(-20, 59), ylim = c(-135, 71), asp = 1)
points(dest.subset$srch_destination_longitude, 
       dest.subset$srch_destination_latitude, col = "red", cex = .1)
points(dest.subset[dest.subset$cluster == 2, ]$srch_destination_longitude, 
       dest.subset[dest.subset$cluster == 2, ]$srch_destination_latitude, col = "blue", cex = .1)
points(dest.subset[dest.subset$cluster == 3, ]$srch_destination_longitude, 
       dest.subset[dest.subset$cluster == 3, ]$srch_destination_latitude, col = "red", cex = .5)
points(dest.subset[dest.subset$cluster == 4, ]$srch_destination_longitude, 
       dest.subset[dest.subset$cluster == 4, ]$srch_destination_latitude, col = "green", cex = .01)
points(dest.subset[dest.subset$cluster == 5, ]$srch_destination_longitude, 
       dest.subset[dest.subset$cluster == 5, ]$srch_destination_latitude, col = "green", cex = .1)
points(dest.subset[dest.subset$cluster == 6, ]$srch_destination_longitude, 
       dest.subset[dest.subset$cluster == 6, ]$srch_destination_latitude, col = "red", cex = .3)


coastal_cluster <- dest.subset[dest.subset$ cluster == 3, ]
write.csv(coastal_cluster, file = "coastal.csv")

major_urban <- dest.subset[dest.subset$ cluster == 5, ][, 1:5]
write.csv(major_urban, file = "urban.csv")

# Covariance Matrix
pop.covariance <- cov(popularity)
pop.correlation <- cor(popularity)


cluster.5.pca <- prcomp(popularity.5, 
                        center = TRUE,
                        scale. = TRUE)

nature <- subset(dest.subset[dest.subset$cluster == 8, ], select = c(-srch_destination_id, 
                                             -srch_destination_name, 
                                             -srch_destination_type_id, 
                                             -srch_destination_longitude, 
                                             -srch_destination_latitude,
                                             -cluster))

nature <- dest.subset[dest.subset$cluster == 8, ]
xkcd.corpus <- Corpus(DataframeSource(data.frame(nature$srch_destination_name)))
xkcd.corpus <- tm_map(xkcd.corpus, removePunctuation)
xkcd.corpus <- tm_map(xkcd.corpus, tolower)
xkcd.corpus <- tm_map(xkcd.corpus, content_transformer(tolower))
xkcd.corpus <- tm_map(xkcd.corpus, PlainTextDocument)
xkcd.corpus <- tm_map(xkcd.corpus, function(x) removeWords(x, stopwords("english")))
xkcd.corpus <- tm_map(xkcd.corpus, function(x) removeWords(x, c("UNITED", "STATES", "AMERICA")))
xkcd.corpus <- tm_map(xkcd.corpus, function(x) removeWords(x, c("UNITED", "STATES", "AMERICA", "UNITED STATES OF AMERICA", "united", "states", "america", "United", "States", "America")))
tdm <- TermDocumentMatrix(xkcd.corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("wordcloud.png", width=1600,height=900)
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()
  for (i in 1:dim(coastal)[2]) {
    coastal[, i] <- 10^coastal[, i]
    xs <- coastal[, i]
    min_x <- max(xs)
    max_x <- min(xs)
    coastal[, i] <- normalize(xs, min_x, max_x) * 100
  }

# PCA
plot_pca_g <- function(df.pca) {
  g <- ggbiplot(df.pca, obs.scale = 1, var.scale = 1, 
                ellipse = TRUE, 
                circle = TRUE
                )
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', 
                 legend.position = 'top')
  
  return(g) 
}

plot_pca <- function(df.pca) {
  theta <- seq(0,2*pi,length.out = 100)
  circle <- data.frame(x = cos(theta), y = sin(theta))
  p <- ggplot(circle,aes(x,y)) + geom_path()
  
  loadings <- data.frame(df.pca$rotation, 
                         .names = row.names(df.pca$rotation))
  p + geom_text(data=loadings, 
                mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
    coord_fixed(ratio=1) +
    labs(x = "PC1", y = "PC2")
  return(p)
}

normalize <- function(x, min, max) {
  return((x-min) / (max - min))
}
