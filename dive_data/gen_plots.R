library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggrepel)
library(ggpattern)


setwd('~/hj_barton/dive_data')

th <- theme_grey(base_size = 12)

dive_dat <- read.csv('dives.csv')
dive_dat <- separate(dive_dat, date, c('day', 'month', 'year'), sep='_', remove=FALSE)
dive_dat <- separate(dive_dat, duration, c('hours', 'minutes'), sep=':', remove=FALSE)
dive_dat$config = ifelse(dive_dat$equipment=='JJ CCR', 'CCR', 'OC')

# get hours numeric column
dive_dat$dec_hours = as.numeric(dive_dat$hours) + (as.numeric(dive_dat$minutes)/60)

# calc rmv and sac
dive_dat$rmv = ((dive_dat$start_pres - dive_dat$end_pres) * dive_dat$volume)/ (dive_dat$dec_hours * 60)
dive_dat$sac = dive_dat$rmv / ((dive_dat$avg_depth / 10) + 1)

#dive_dat$year_month <- paste(dive_dat$year, dive_dat$month, sep='_')
dive_dat$year_month <- 2000 + as.numeric(dive_dat$year) + (as.numeric(dive_dat$month)/12)

hist_dat <- summarise(group_by(dive_dat, year_month, environment), num_dives=length(duration))
course_data <- distinct(subset(dive_dat, course!='' & course!='DM', select=c('year_month', 'course', 'environment')),
                        course, .keep_all =T )
total_dives_per_col <- summarise(group_by(dive_dat, year_month), num_dives=length(duration))

course_data <- dplyr::left_join(course_data, total_dives_per_col)

# dive history by type
hist <- ggplot(hist_dat, aes(x=year_month, y=num_dives, fill=environment))+
    geom_bar(stat='identity') +
    geom_label_repel(data=course_data, aes(x=year_month, y=num_dives+2, label=course),
                     fill='grey', direction='y', segment.color=NA, alpha=0.7) +
    scale_fill_viridis(discrete=T) + th +
    theme(legend.title=element_blank(),
        legend.position = "bottom",
        legend.margin = margin(t = -4, r = 0, b = 2, l = -10, unit = "mm")) +
    xlab('') + ylab('Number of dives') +
    guides(fill = guide_legend(nrow = 1))

# RMV
rmv <- ggplot(subset(dive_dat, config=='CCR'), aes(x=config, y=rmv)) +
  geom_boxplot() + labs(x='\n', y='Oxygen consumption (lpm) - CCR') +th +
  ylim(0, 3.5)

# depth dist
depths <- ggplot(dive_dat, aes(x=max_depth, fill=config)) +
  geom_histogram(binwidth=2.5) +
  scale_fill_viridis(discrete = T) +th +
  labs(x='Depth (m)\n', y='Number of dives') +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.7))

depths_env <- ggplot(dive_dat, aes(x=max_depth, fill=environment)) +
  geom_histogram(binwidth=2.5) +
  scale_fill_viridis(discrete = T) +th +
  labs(x='Depth (m)\n', y='Number of dives') +
  theme(legend.title = element_blank(), legend.position = 'none')

# hours summary
total_dat <- subset(dive_dat, select = c('environment', 'dec_hours', 'config'))
total_hrs <- summarise(group_by(total_dat, config), dec_hours=sum(dec_hours))
total_hrs$environment = 'total'

total_dat <- dplyr::full_join(total_dat, total_hrs)
total_dat$environment <- factor(total_dat$environment,
                                levels=c('total', 'quarry', 'mine', 'lake', 'cave', 'wreck', 'reef', 'seabed'))

hours_env_config <- ggplot(total_dat, aes(x=environment, y=dec_hours, fill=config)) +
  geom_bar(stat='identity') +
  scale_fill_viridis(discrete = T) +th +
  labs(x='', y='Total hours')+
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.7), axis.text.x = element_text(angle=45, hjust=1))


sac <- ggplot(subset(dive_dat, config!='CCR'), aes(x=config, y=sac)) +
  geom_boxplot() + th + labs(x='\n', y='Surface air consumption (lpm) - OC')

# year summaries
year_dat <- summarise(group_by(dive_dat, year, environment, config), hours=sum(dec_hours))

year_plot <- ggplot(year_dat, aes(x=as.character(2000+as.numeric(year)), y=hours, fill=environment, alpha=config)) +
  geom_bar(stat = 'identity', colour='black') +
  scale_alpha_manual(values=c(1, 0.6)) +
  scale_fill_viridis(discrete = T) +th +
  coord_flip()+
  theme(legend.title=element_blank(), legend.margin = margin(-0.2,0,-0.2,0, unit="cm")) +
  labs(x='')

png('year_summary.png', width=12, height=3, res=320, units='in')

year_plot

dev.off()

png('dive_summaries.png', width=12, height=6.5, res=320, units='in')

layout <- rbind(c(1,1,1,1,1,1,1,1),
                c(1,1,1,1,1,1,1,1),
                c(1,1,1,1,1,1,1,1),
                c(2,2,3,3,4,4,5,6),
                c(2,2,3,3,4,4,5,6))

grid.arrange(hist, hours_env_config, depths_env, depths, sac, rmv, layout_matrix=layout)

dev.off()