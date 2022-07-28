######################################################################################
#####                                                                            ##### 
#####    Supplementary material for Fried 2016, Journal of Affective Disorders   ##### 
#####                                                                            ##### 
#####                               Main analysis                                #####
#####                                                                            ##### 
######################################################################################


library('ggplot2')
library('tidyverse')
library('bruceR')
##### Data preparation

dataplot = bruceR::import("C:\\Users\\hmz19\\Desktop\\task2\\R\\MatrixA_plot.csv")  #Load data for plot (difference between specific and compound symptoms)


##### Figure 1 
##### Thanks for assistance with ggplot2 to Jana Jarecki
set.seed(223)
d <- dataplot %>%
  dplyr::select(.,-S_name) %>%
  dplyr::mutate(.,S = paste(1:nrow(dataplot),sep = "")) %>%
  melt(., id.vars="S", variable.name="Scales", value.name="Type") %>%
  dplyr::filter(.,Type >= 1) %>%
  dplyr::mutate(.,Type = ifelse(Type==1,"Scale contains compound symptom","Scale contains specific symptom")) %>%
  dplyr::mutate(.,S=as.numeric(S))

symp <- dataplot %>%
  dplyr::mutate(.,S=1:nrow(.)) %>%
  dplyr::mutate(S_name = factor(S_name,levels = .$S_name))  %>%
  dplyr::select(S,S_name)

#levels(symp$S_name)

num = d %>%
  dplyr::mutate(n=1) %>%
  dplyr::group_by(Scales) %>%
  dplyr::summarise(.,count = sum(n)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(.,desc(count)) %>%
  dplyr::mutate(order=1:nrow(.))


plot = dplyr::full_join(num, d, by = "Scales") %>%
  dplyr::mutate(Scales2=order) %>%
  dplyr::mutate(Scales = factor(Scales,levels = num$Scales))  %>%
  dplyr::mutate(S = factor(S,levels = symp$S,labels = symp$S_name))  %>%
  dplyr::select(S,Scales,Scales2,Type)

#levels(plot$Scales)

# Plot
pal1 <- c("#DB6968", "#F8984E", "#4D97CD", "#D4D66D", "#60BC55", "#88C4E8", "#FCBC7E")
#pal2 <- viridis(7)
#pal3 <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628")

a<- ggplot(plot, aes(x=S, y=Scales2, group=S, color=Scales, shape=Type, rev=F)) +
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
  theme(axis.text.x = element_text(angle=90, hjust=1),
        axis.text.y = element_text(angle=120, hjust=1),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position="right",
        plot.margin = unit(rep(.5,4), "lines")) +
  labs(fill="") + # remove legend title
  scale_y_continuous(limits=c(0,7.5), expand=c(0,0), breaks=1:7, labels=levels(plot$Scales))+
  scale_color_manual(values=pal1);  

a

ggsave(plot=a,filename="Figure1.pdf", width=20, height=11.5, useDingbats=FALSE)
# Figure 1 was further adjusted in Inkscape

