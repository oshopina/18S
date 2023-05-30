## Make Heatmap of OTUs using 16S OTUs >= 0.5% relative abundance
## Only relative abundance is used

##load data
otu.hm <- read.table('Data/otu_18S_4350.csv', header=TRUE, sep=',', row.names=1)
otu.env <- read.table('Data/env_18S.csv', header=TRUE, sep=',', row.names=1)

##calculate relative abundance
otu.hm<- as.data.frame(t(otu.hm/4350))
rowSums(otu.hm)

##select OTUs with value more than 0.05 in at least one column
otu.hm <- otu.hm[,colSums(otu.hm > 0.05) > 0] %>%
  as.matrix()

# Make palette and heatmap
mypal <- colorRampPalette(c("White","Black"))

heatmap(sqrt((otu.hm)),Rowv=NA,Colv=NA,col=mypal(256),revC=TRUE,scale='none')

# Make the pH gradient
mypal.pH <- colorRampPalette(c("#9e0142",
                               "#d53e4f",
                               "#f46d43",
                               "#fdae61",
                               "#fee08b",
                               "#e6f598",
                               "#abdda4",
                               "#66c2a5",
                               "#3288bd",
                               "#5e4fa2"))

otu.pH <- data.frame(pH1 = otu.env$pH, ph2 = otu.env$pH)
legend.pH <- data.frame(pH1 = 1:88, ph2 = 1:88)

heatmap(as.matrix(t(otu.pH)),Rowv=NA,Colv=NA,col=mypal.pH(256),revC=TRUE,scale='none')
heatmap(as.matrix(t(legend.pH)),Rowv=NA,Colv=NA,col=mypal.pH(256),revC=TRUE,scale='none')
