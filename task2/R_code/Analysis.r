######################################################################################
#####                                                                            ##### 
#####    Supplementary material for Fried 2016, Journal of Affective Disorders   ##### 
#####                                                                            ##### 
#####                               Main analysis                                #####
#####                                                                            ##### 
######################################################################################

library('qgraph')
library('ggplot2')
library('data.table')
library('reshape2')
library('psych')
library('ade4')
#install.packages('viridisLite')
library('viridisLite')
library('viridis')



##### Data preparation
data = fread("C:\\Users\\hmz19\\Desktop\\task2\\R\\MatrixB.csv")     #Load data for estimating Jaccard index (no difference between specific and compound symptoms)
dataplot = fread("C:\\Users\\hmz19\\Desktop\\task2\\R\\MatrixA.csv") #Load data for plot (difference between specific and compound symptoms)

##### Estimation of overlap, using the Jaccard Index

# IDS
data1<-data[which(data$HRSD==1|data$IDS==1),]
a1<-1-(dist.binary(matrix(c(data$IDS ,data$QIDS),nrow=2,byrow=T), method = 1)^2); a1
b1<-1-(dist.binary(matrix(c(data$IDS ,data$BDI),nrow=2,byrow=T), method = 1)^2)
c1<-1-(dist.binary(matrix(c(data$IDS ,data$CESD),nrow=2,byrow=T), method = 1)^2)
d1<-1-(dist.binary(matrix(c(data$IDS ,data$SDS),nrow=2,byrow=T), method = 1)^2)
e1<-1-(dist.binary(matrix(c(data$IDS ,data$MADRS),nrow=2,byrow=T), method = 1)^2)
f1<-1-(dist.binary(matrix(c(data$IDS ,data$HRSD),nrow=2,byrow=T), method = 1)^2)
IDS.v<-c(1,a1,b1,c1,d1,e1,f1)

# QIDS
a2<-1-(dist.binary(matrix(c(data$QIDS, data$IDS),nrow=2,byrow=T), method = 1)^2)
b2<-1-(dist.binary(matrix(c(data$QIDS ,data$BDI),nrow=2,byrow=T), method = 1)^2)
c2<-1-(dist.binary(matrix(c(data$QIDS ,data$CESD),nrow=2,byrow=T), method = 1)^2)
d2<-1-(dist.binary(matrix(c(data$QIDS ,data$SDS),nrow=2,byrow=T), method = 1)^2)
e2<-1-(dist.binary(matrix(c(data$QIDS ,data$MADRS),nrow=2,byrow=T), method = 1)^2)
f2<-1-(dist.binary(matrix(c(data$QIDS ,data$HRSD),nrow=2,byrow=T), method = 1)^2)
QIDS.v<-c(a2,1,b2,c2,d2,e2,f2)

# BDI
a3<-1-(dist.binary(matrix(c(data$BDI, data$IDS),nrow=2,byrow=T), method = 1)^2)
b3<-1-(dist.binary(matrix(c(data$BDI, data$QIDS),nrow=2,byrow=T), method = 1)^2)
c3<-1-(dist.binary(matrix(c(data$BDI, data$CESD),nrow=2,byrow=T), method = 1)^2)
d3<-1-(dist.binary(matrix(c(data$BDI, data$SDS),nrow=2,byrow=T), method = 1)^2)
e3<-1-(dist.binary(matrix(c(data$BDI ,data$MADRS),nrow=2,byrow=T), method = 1)^2)
f3<-1-(dist.binary(matrix(c(data$BDI ,data$HRSD),nrow=2,byrow=T), method = 1)^2)
BDI.v<-c(a3,b3,1,c3,d3,e3,f3)

# CESD
a4<-1-(dist.binary(matrix(c(data$CESD, data$IDS),nrow=2,byrow=T), method = 1)^2)
b4<-1-(dist.binary(matrix(c(data$CESD, data$QIDS),nrow=2,byrow=T), method = 1)^2)
c4<-1-(dist.binary(matrix(c(data$CESD, data$BDI),nrow=2,byrow=T), method = 1)^2)
d4<-1-(dist.binary(matrix(c(data$CESD, data$SDS),nrow=2,byrow=T), method = 1)^2)
e4<-1-(dist.binary(matrix(c(data$CESD ,data$MADRS),nrow=2,byrow=T), method = 1)^2)
f4<-1-(dist.binary(matrix(c(data$CESD ,data$HRSD),nrow=2,byrow=T), method = 1)^2)
CESD.v<-c(a4,b4,c4,1, d4,e4,f4)

# SDS
a5<-1-(dist.binary(matrix(c(data$SDS, data$IDS),nrow=2,byrow=T), method = 1)^2)
b5<-1-(dist.binary(matrix(c(data$SDS, data$QIDS),nrow=2,byrow=T), method = 1)^2)
c5<-1-(dist.binary(matrix(c(data$SDS, data$BDI),nrow=2,byrow=T), method = 1)^2)
d5<-1-(dist.binary(matrix(c(data$SDS, data$CESD),nrow=2,byrow=T), method = 1)^2)
e5<-1-(dist.binary(matrix(c(data$SDS ,data$MADRS),nrow=2,byrow=T), method = 1)^2)
f5<-1-(dist.binary(matrix(c(data$SDS ,data$HRSD),nrow=2,byrow=T), method = 1)^2)
SDS.v<-c(a5,b5,c5, d5,1, e5,f5)

# MADRS
a6<-1-(dist.binary(matrix(c(data$MADRS, data$IDS),nrow=2,byrow=T), method = 1)^2)
b6<-1-(dist.binary(matrix(c(data$MADRS, data$QIDS),nrow=2,byrow=T), method = 1)^2)
c6<-1-(dist.binary(matrix(c(data$MADRS, data$BDI),nrow=2,byrow=T), method = 1)^2)
d6<-1-(dist.binary(matrix(c(data$MADRS, data$CESD),nrow=2,byrow=T), method = 1)^2)
e6<-1-(dist.binary(matrix(c(data$MADRS ,data$SDS),nrow=2,byrow=T), method = 1)^2)
f6<-1-(dist.binary(matrix(c(data$MADRS ,data$HRSD),nrow=2,byrow=T), method = 1)^2)
MADRS.v<-c(a6,b6,c6, d6,e6,1,f6)

# HRSD
a7<-1-(dist.binary(matrix(c(data$HRSD, data$IDS),nrow=2,byrow=T), method = 1)^2)
b7<-1-(dist.binary(matrix(c(data$HRSD, data$QIDS),nrow=2,byrow=T), method = 1)^2)
c7<-1-(dist.binary(matrix(c(data$HRSD, data$BDI),nrow=2,byrow=T), method = 1)^2)
d7<-1-(dist.binary(matrix(c(data$HRSD, data$CESD),nrow=2,byrow=T), method = 1)^2)
e7<-1-(dist.binary(matrix(c(data$HRSD ,data$SDS),nrow=2,byrow=T), method = 1)^2)
f7<-1-(dist.binary(matrix(c(data$HRSD ,data$MADRS),nrow=2,byrow=T), method = 1)^2)
HRSD.v<-c(a7,b7,c7, d7,e7,f7,1)

# Create table
M = matrix(nrow=7, ncol=7) 
colnames(M) <- c("IDS",	"QIDS",	"BDI",	"CESD",	"SDS"	,"MADRS"	,"HRSD")
rownames(M) <- c("IDS",	"QIDS",	"BDI",	"CESD",	"SDS"	,"MADRS"	,"HRSD")
M[1,]<-IDS.v
M[2,]<-QIDS.v
M[3,]<-BDI.v
M[4,]<-CESD.v
M[5,]<-SDS.v
M[6,]<-MADRS.v
M[7,]<-HRSD.v
isSymmetric(M)

M
M[M == 1] <- 0 # replace diagonal with 0
colMeans(M)
mean(colMeans(M)) #0.36

# ---------- beginning of new code part 1, August 28 2018
x <- colMeans(M)
x[1] <- sum(M[,1])/6
x[2] <- sum(M[,2])/6
x[3] <- sum(M[,3])/6
x[4] <- sum(M[,4])/6
x[5] <- sum(M[,5])/6
x[6] <- sum(M[,6])/6
x[7] <- sum(M[,7])/6

# IDS      QIDS       BDI      CESD       SDS     MADRS      HRSD 
# 0.3941210 0.4027273 0.3972875 0.2661492 0.3682781 0.2966050 0.3599546  # with diag=0, as reported in paper
# 0.5369781 0.5455844 0.5401447 0.4090063 0.5111353 0.4394622 0.5028117  # with diag=1, which would be wrong as well (inflate similarity)
# 0.4598078 0.4698485 0.4635021 0.3105074 0.4296578 0.3460392 0.4199470  # without the diagonal, which should be correct

mean(colMeans(M)) #0.36 with diag=0, as reported in paper
mean(colMeans(M)) #0.50 with diag=1, which would be wrong as well (inflate similarity)
mean(x)           #0.41 without the diag, which is correct

# ---------- end of new code part 1, August 28 2018

length1<-c(28,9,21,18,20,9,17) # length of original (adjusted) questionnaires; e.g. CESD 20 items originally, combined down to 18
length2<-c(33,20,25,21,23,12,22) # items in analysis per scale; CESD captures 21 items

cor(length1, colMeans(M)) #0.2938447
cor(length2, colMeans(M)) #0.5720158

##### Figure 1 
##### Thanks for assistance with ggplot2 to Jana Jarecki

#install.packages("data.table")
#library('data.table')
set.seed(223)
d <- dataplot
d[, S := factor(paste0("S",1:nrow(d)))] #Create symptom variable
d = melt(d, id.vars="S", variable.name="Scales", value.name="Type") #Transform to long format
d = d["Type">=1] #Keep the scales in which the symptoms are 1 (present) or 2 (included)
d[, Type := factor(Type, labels=c("Scale contains compound symptom", "Scale contains specific symptom"))]
d[, count := .N, by=S]

# Symptom order
sympt.order = d[, .N, by=S][order(N)][, S] #Replace by order
d[, S := factor(S, levels = sympt.order)]

# Scale order by frequency
scale.order = d[, .N, by=Scales][order(N)][, Scales]
d[, Scales := factor(Scales, levels = scale.order)]
d[, Scales2 := as.numeric(Scales)]

# Plot
pal1 <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# pal2 <- viridis(7)
# pal3 <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628")

a<- ggplot(d, aes(x=S, y=Scales2, group=S, color=as.factor(Scales), shape=Type, rev=F)) +
  geom_line() + #keep this here, otherwise there is an error 
  xlab("") +
  ylab("") +
  # Generate the grid lines
  geom_hline(yintercept = 1:7, colour = "grey80", size = .2) +
  geom_vline(xintercept = 1:52, colour = "grey80", size = .2) +
  # Points and lines
  geom_line(colour="grey60") +
  geom_point(size=3, fill="white") +
  # Fill the middle space with a white blank rectangle
  geom_rect(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=.6,fill="white", color=NA) +
  # Polar coordinates
  coord_polar() +
  scale_shape_manual(values=c(21,19)) +
  # The angle for the symptoms and remove the default grid lines
  theme(axis.text.x = element_text(angle = 360/(2*pi)*rev( pi/2 + seq( pi/52, 2*pi-pi/52, len=52)) + c(rep(0, 26), rep(180,28))),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position="top",
        plot.margin = unit(rep(.5,4), "lines")) +
  labs(fill="") + # remove legend title
  scale_y_continuous(limits=c(-4,7), expand=c(0,0), breaks=1:7, labels=d[, levels(Scales)]) +
  scale_color_manual(values=pal1); a

ggsave(plot=a,filename="Figure1.pdf", width=7, height=7, useDingbats=FALSE)
# Figure 1 was further adjusted in Inkscape


##### Figure 2
adjacency<-read.csv("MatrixB.csv", head=F)
adjacency<-adjacency[-1,]
adjacency<-as.data.frame(lapply(adjacency,as.numeric))

adjacency$V1[adjacency$V1== 1] <- 0
adjacency$V1[adjacency$V1== 2] <- 1
adjacency$V2[adjacency$V2== 1] <- 0
adjacency$V2[adjacency$V2== 2] <- 1
adjacency$V3[adjacency$V3== 1] <- 0
adjacency$V3[adjacency$V3== 2] <- 1
adjacency$V4[adjacency$V4== 1] <- 0
adjacency$V4[adjacency$V4== 2] <- 1
adjacency$V5[adjacency$V5== 1] <- 0
adjacency$V5[adjacency$V5== 2] <- 1
adjacency$V6[adjacency$V6== 1] <- 0
adjacency$V6[adjacency$V6== 2] <- 1
adjacency$V7[adjacency$V7== 1] <- 0
adjacency$V7[adjacency$V7== 2] <- 1

adjacency<-as.matrix(adjacency)
adj <-  1*(adjacency%*%t(adjacency)) 
diag(adj) = 0

pdf("Figure2.pdf", useDingbats=FALSE)
Fig2<-qgraph(adj, layout="spring", vsize=3, cut=7, border.width=1.5, 
             border.color="black", repulsion=.8) 
dev.off()