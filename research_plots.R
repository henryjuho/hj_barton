library(ggplot2)
library(dplyr)
library(viridis)
library(gridExtra)
library(comprehenr)

c1 <- viridis(n=3)
c2 <- adjustcolor(c1, alpha.f=0.5)
colours <- to_vec(for(i in 1:3) c(c1[i], c2[i]))

dmel_i <- data.frame(sel=c('neutral', 'weakly deleterious', 'deleterious', 'strongly deleterious'),
         prop=c(0.0244, 0, 0, 0.976),
         var=c('insertions', 'insertions', 'insertions', 'insertions'),
         spp=c('D. melanogaster', 'D. melanogaster', 'D. melanogaster', 'D. melanogaster'))

dmel_d <- data.frame(sel=c('neutral', 'weakly deleterious', 'deleterious', 'strongly deleterious'),
         prop=c(0.046, 0, 0, 0.954),
         var=c('deletions', 'deletions', 'deletions', 'deletions'),
         spp=c('D. melanogaster', 'D. melanogaster', 'D. melanogaster', 'D. melanogaster'))

gt_i <- data.frame(sel=c('neutral', 'weakly deleterious', 'deleterious', 'strongly deleterious'),
         prop=c(0.03541607, 0, 0, 0.9645839),
         var=c('insertions', 'insertions', 'insertions', 'insertions'),
         spp=c('great tit', 'great tit', 'great tit', 'great tit'))

gt_d <- data.frame(sel=c('neutral', 'weakly deleterious', 'deleterious', 'strongly deleterious'),
         prop=c(0.03882046, 0, 0, 0.9611795),
         var=c('deletions', 'deletions', 'deletions', 'deletions'),
         spp=c('great tit', 'great tit', 'great tit', 'great tit'))

plot_data <- bind_rows(dmel_i, dmel_d, gt_i, gt_d)

plot_data$sel <- factor(plot_data$sel, levels=c('neutral', 'weakly deleterious', 'deleterious', 'strongly deleterious'))

dfe_plot <- ggplot(plot_data, aes(x=sel, y=prop, fill=paste(spp, var, sep=' '))) +
  geom_bar(stat='identity', position = position_dodge(width=0.9), colour='black') +
  scale_fill_manual(values = colours) +
  theme_classic() +
  theme(legend.title=element_blank(),
        legend.position = "top", legend.key.size = unit(.28, "cm"),
        legend.margin = margin(t = 1, r = 0, b = 0, l = -10, unit = "mm"),
        legend.text = element_text(colour = "black",size=7),
        axis.text.y = element_text(colour = "black",size=8),
        axis.text.x = element_text(colour = "black",size=8, angle=25, hjust=1),
        #plot.title = element_text(size=9, vjust=-6, hjust=0.007, face='bold'),
        strip.background = element_blank(),
        plot.margin = margin(t=0, r=5.5, b=2, l=5.5, unit = "pt"))+
  xlab('') + ylab('proportion of variants') +
  guides(fill = guide_legend(nrow = 2))

png('dfe_plot.png', width=3, height=2.75, units='in', res=320)

dfe_plot

dev.off()