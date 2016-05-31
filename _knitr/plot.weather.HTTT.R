HTTT <- read.csv("HTTT.csv")

#load required packages
require(ggplot2)
require(cowplot)

#create dataframe using only year and sex variables
df <- data.frame(table(HTTT$Year,
                       HTTT$Week,
                       HTTT$Sex))

temps<-c(0,0,0,0,71,61,53,69,0,0,0,0,71,64,75,75,0,0,0,0,59,54,57,79,0,0,0,0,0,55,68,62,74,59,0,0,0,0,60,70,61,60)

#plot by week with weather data
p <- ggplot(df[which(df$Freq>0),], aes(x=Var2, y=Freq, fill=Var3))

#barplot
p <- p + geom_bar(stat="identity")

#add temp bars
#p <- p + geom_point(aes(y=temps, group=Var2, size=6), stat="identity", position=position_dodge(width=0.9), alpha=.7, shape=95, stroke=8, color="darkorange2")
#p <- p + geom_line(aes(x=Var2, y=temps, group=Var2))


#facet by year
p <- p + facet_wrap(~Var1, nrow=1, scales="free_x", switch="x")

#plot theme
p <- p + theme(plot.title=element_text(family="Helvetica", 
                                       face="bold", 
                                       size=36, 
                                       color="firebrick4", 
                                       vjust=20), 
               axis.text.x=element_blank(),
               axis.title.y=element_text(margin=margin(0,17,0,0), 
                                         family="Helvetica", 
                                         face="bold",
                                         size=14),
               axis.text.y=element_text(family="Helvetica",
                                        size=14), 
               axis.ticks.x=element_blank(),
               legend.text=element_text(size=14, 
                                        face="bold"), 
               plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
               legend.position="bottom",
               #panel.grid.major=element_line(color="gray90"),
               #panel.grid.minor=element_line(color="gray90"),
               #panel.grid.major.x=element_blank(),
               #panel.grid.minor.x=element_blank(),
               #change year labels
               strip.text.x=element_text(size=14, face="bold"),
               #increase distance between panels
               panel.margin.x=unit(1, "lines"))

#y axis title, plot title
p <- p + labs(y="# Racers", 
              x=NULL, 
              title=NULL)

#temperature labels
labels <- c("","","","","71","61","53","69","","","","","71","64","75","75","","","","","59","54","57","79","","","","","","55","68","62", "74","59","","","","","60", "70","61","60")

heights <- c(0,0,0,0,71,61,53,69,0,0,0,0,71,64,75,75,0,0,0,0,59,54,57,79,0,0,0,0,0,55,68,62,74,59,0,0,0,0,60,70,61,60)

#create value labels in bar
p <- p + geom_text(position="stack",aes(label=labels, y=heights), size=4, family="Helvetica",color="chocolate2")

#remove legend title
p <- p + guides(fill=guide_legend(title=NULL))

#set bar colors
p <- p + scale_fill_manual(labels=c("Women","Men"),
                           values=c("firebrick4","gray25"))

#remove space between 0 and x axis
p <- p + scale_y_continuous(limits=c(0,85), expand=c(0,0))
p


