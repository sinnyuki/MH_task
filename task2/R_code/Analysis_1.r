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
library('viridis')
library('tidyverse')

##### Data preparation
data = fread("C:\\Users\\hmz19\\Desktop\\task2\\R\\MatrixB_1.csv")     #Load data for estimating Jaccard index (no difference between specific and compound symptoms)
dataplot = fread("C:\\Users\\hmz19\\Desktop\\task2\\R\\MatrixA_1.csv")  #Load data for plot (difference between specific and compound symptoms)


##### Figure 1 
##### Thanks for assistance with ggplot2 to Jana Jarecki
set.seed(223)
d <- dataplot %>%
  dplyr::mutate(.,S = paste("S",1:nrow(dataplot),sep = "")) %>%
  melt(., id.vars="S", variable.name="Scales", value.name="Type") %>%
  dplyr::filter(.,Type >= 1) %>%
  dplyr::mutate(.,Type = ifelse(Type==1,"Scale contains compound symptom","Scale contains specific symptom")) 

#d[, S := factor(paste0("S",1:nrow(d)))] #Create symptom variable

#d =  melt(d, id.vars="S", variable.name="Scales", value.name="Type")#Transform to long format

#d = d["Type">=1] #Keep the scales in which the symptoms are 1 (present) or 2 (included)
#d = dplyr::filter(d,Type >= 1)

#d[, 'Type' := factor('Type', labels=c("Scale contains compound symptom", "Scale contains specific symptom"))]
#d = dplyr::mutate(d,Type = ifelse(Type==1,"Scale contains compound symptom","Scale contains specific symptom")) 

#d[, count := .N, by=S] #

num = d %>%
  dplyr::mutate(n=1) %>%
  dplyr::group_by(Scales) %>%
  dplyr::summarise(.,count = sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(.,desc(count)) %>%
  dplyr::mutate(order=1:nrow(.))

# Symptom order
#sympt.order = d[, .N, by=S][order(N)][, S] #Replace by order
#d[, S := factor(S, levels = sympt.order)]

# Scale order by frequency
#scale.order = d[, .N, by=Scales][order(N)][, Scales]
#d[, Scales := factor(Scales, levels = scale.order)]
#d[, Scales2 := as.numeric(Scales)]

plot = dplyr::full_join(num, d, by = "Scales") %>%
        dplyr::mutate(Scales2=order) %>%
        dplyr::mutate(Scales = factor(Scales))  %>%
        dplyr::select(S,Scales,Scales2,Type)

#levels(plot$Scales)

# Plot
pal1 <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#pal2 <- viridis(7)
#pal3 <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628")

a<- ggplot(plot, aes(x=S, y=Scales2, group=S, color=as.factor(Scales), shape=Type, rev=F)) +
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
  #circle or not
  #coord_polar() +
  scale_shape_manual(values=c(21,19)) +
  # The angle for the symptoms and remove the default grid lines
  theme(axis.text.x = element_text(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position="top",
        plot.margin = unit(rep(.5,4), "lines")) +
  labs(fill="") + # remove legend title
  scale_y_continuous(limits=c(-4,7.5), expand=c(0,0), breaks=1:7, labels=levels(plot$Scales)) +
  scale_color_manual(values=pal1);  

a

ggsave(plot=a,filename="Figure1.pdf", width=20, height=11.5, useDingbats=FALSE)
# Figure 1 was further adjusted in Inkscape

