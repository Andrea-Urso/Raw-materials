# initial setup

suppressWarnings(library(tidyverse))  # data manipulation
suppressWarnings(library(readr))      # data reading
suppressWarnings(library(cluster))    # clustering algorithms
suppressWarnings(library(factoextra)) # clustering visualization
suppressWarnings(library(dendextend)) # for comparing two dendrograms
setwd('/home/andrea/Cellulose')       # set work directory

# load files

suppressWarnings(SP1 <- read_csv("Southern_pine_lot_1.csv"))
SP1$X1 <- NULL
suppressWarnings(SP2 <- read_csv("Southern_pine_lot_2.csv"))
SP2$X1 <- NULL
suppressWarnings(BRC <- read_csv("Birch.csv"))
BRC$X1 <- NULL
suppressWarnings(ECL <- read_csv("Eucaliptus.csv"))
ECL$X1 <- NULL
suppressWarnings(EPW <- read_csv("Eucaliptus_mixed.csv"))
EPW$X1 <- NULL
suppressWarnings(SHE <- read_csv("southern_hemisphere_eucaliptus.csv"))
SHE$X1 <- NULL
suppressWarnings(CTM <- read_csv("CTMP_Birch.csv"))
CTM$X1 <- NULL
suppressWarnings(POP <- read_csv("Poplar.csv"))
POP$X1 <- NULL
suppressWarnings(NBT <- read_csv("NBSK_TCF.csv"))
NBT$X1 <- NULL

# merge files in complessive dataframe

SP1_SUB <- subset(SP1, select = c('Name', 'OTU'))
BC <- SP1_SUB
names(BC)[names(BC) == 'OTU'] <- 'Southern pine lot 1'

SP2_SUB <- subset(SP2, select = c('Name', 'OTU'))
BC <- merge(BC,SP2_SUB, by = 'Name', all = TRUE)
names(BC)[names(BC) == 'OTU'] <- 'Southern pine lot 2'

BRC_SUB <- subset(BRC, select = c('Name', 'OTU'))
BC <- merge(BC,BRC_SUB, by = 'Name', all = TRUE)
names(BC)[names(BC) == 'OTU'] <- 'Birch'

ECL_SUB <- subset(ECL, select = c('Name', 'OTU'))
BC <- merge(BC,ECL_SUB, by = 'Name', all = TRUE)
names(BC)[names(BC) == 'OTU'] <- 'Eucaliptus'

EPW_SUB <- subset(EPW, select = c('Name', 'OTU'))
BC <- merge(BC,EPW_SUB, by = 'Name', all = TRUE)
names(BC)[names(BC) == 'OTU'] <- 'Eucaliptus Mixed'

SHE_SUB <- subset(SHE, select = c('Name', 'OTU'))
BC <- merge(BC,SHE_SUB, by = 'Name', all = TRUE)
names(BC)[names(BC) == 'OTU'] <- 'Southern hem. eucaliptus'

CTM_SUB <- subset(CTM, select = c('Name', 'OTU'))
BC <- merge(BC,CTM_SUB, by = 'Name', all = TRUE)
names(BC)[names(BC) == 'OTU'] <- 'CTMP Birch'

POP_SUB <- subset(POP, select = c('Name', 'OTU'))
BC <- merge(BC,POP_SUB, by = 'Name', all = TRUE)
names(BC)[names(BC) == 'OTU'] <- 'Poplar'

NBT_SUB <- subset(NBT, select = c('Name', 'OTU'))
BC <- merge(BC,NBT_SUB, by = 'Name', all = TRUE)
names(BC)[names(BC) == 'OTU'] <- 'NBSK TCF'

# transpose and prepare dataframe for clustering

BC[is.na(BC)] <- 0.0
BC$Name <- NULL
BCT <- t(BC)
BCT_sc <- as.data.frame(scale(BCT))

# cluster dendrogram plotting (K = 3)

d <- dist(BCT_sc, method = "euclidean")
hc1 <- hclust(d, method = "average" )
sub_grp <- cutree(hc1, k = 3)
table(sub_grp)
BCT_sc %>%
  mutate(cluster = sub_grp) %>%
  head
plot(hc1, cex = 1.5, hang = -1, main = 'Cluster Dendrogram of Cellulose')
rect.hclust(hc1, k = 3, border = 2:4)
box(which = 'plot', lty = 'solid')

# cluster plot

fviz_cluster(list(data = BCT_sc, cluster = sub_grp), ellipse = TRUE, ellipse.type = "norm")
