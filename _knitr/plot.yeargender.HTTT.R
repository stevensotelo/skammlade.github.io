#load HTTT csv file
HTTT <- read.csv("HTTT.csv")
#attach(HTTT)
#load required packages
require(ggplot2)
require(cowplot)

#create dataframe using only year and sex variables

df<-data.frame(table(HTTT$Year,HTTT$Sex))

#barplot
p <- ggplot(df, aes(Var1, Freq, group=Var2,fill=Var2))
p <- p + geom_bar(width=0.75, 
                  position=position_dodge(width=0.78),
                  stat="identity")

#theme
p <- p + theme(plot.title=element_text(family="Helvetica", 
                                       face="bold", 
                                       size=36, 
                                       color="firebrick4", 
                                       vjust=20), 
               axis.text.x=element_text(family="Helvetica",
                                        size=14, face="bold"),
               axis.title.y=element_text(margin=margin(0,17,0,0), 
                                         family="Helvetica",
                                         size=14, face="bold"),
               axis.text.y=element_text(family="Helvetica", 
                                        size=14), 
               legend.text=element_text(size=14, face="bold"), 
               legend.position="bottom",
               plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
               axis.ticks.x=element_blank())

#y axis title, plot title
p <- p + labs(y="# Racers", 
              x=NULL)
#create value labels in bar
p <- p + geom_text(data=subset(df,Freq !=0),
                   aes(label=Freq), 
                   position=position_dodge(width=0.75), 
                   vjust=1.3, 
                   size=4.5, 
                   family="Helvetica",
                   color="white")

#remove legend title
p <- p + guides(fill=guide_legend(title=NULL))

#set y axis limits
ylim=trunc(max(df$Freq)+10)
p <- p + scale_y_continuous(limits=c(0,ylim),expand=c(0,0))

#set bar colors
p <- p + scale_fill_manual(labels=c("Women","Men"),
                   values=c("firebrick4","gray25")) 

print(p)

