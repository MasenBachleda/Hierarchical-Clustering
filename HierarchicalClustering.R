setwd("~/Desktop/854datasets")
pharm <- read.csv("Pharmaceuticals.csv")
# Part a
row.names(pharm) <- pharm[,2]
pharm.num <- pharm[, c(3,4,5,6,7,8,9,10,11)]
row.names(pharm.num) <- row.names(pharm)
pharm.num.norm <- sapply(pharm.num, scale)
row.names(pharm.num.norm) <- row.names(pharm)
# I decided to normalize the data because I will be using euclidean distance and I didn't want to sway the distances with values like market cap that are relatively higher 
d <- dist(pharm[, c(3,4,5,6,7,8,9,10,11)], method="euclidean")
d.norm <- dist(pharm.num.norm, method="euclidean")

hc2 <- hclust(d.norm, method="average")
plot(hc2, hang=-1, ann=FALSE)
# I decided to use hierarchical clustering with average linkage because it is one of the most popular linkage types and we are clustering a smaller number of rows
# After looking at the dendrogram with average linkage I decided that 3 or 5 clusters would be best and after comparing them I went with 3 because it appears to encapsulate the same variation

# Part b
memb3 <- cutree(hc2, k=3)
meanAgg3 = aggregate(memb3, pharm.num, FUN=mean)
meanAgg3

# Part c and d
MRTab2 = table(pharm$Median_Recommendation, memb3)
colnames(MRTab2)=c("c1","c2","c3")
MRTab2

LTab2 = table(pharm$Location, memb3)
colnames(LTab2)=c("c1","c2","c3")
LTab2

ETab2 = table(pharm$Exchange, memb3)
colnames(ETab2)=c("c1","c2","c3")
ETab2
#c1 (size 15) largest with mostly US Location
#c2 (size 3) is all NYSE and 2 Hold
#c3 (size 3) has 2 Moderate Buy










