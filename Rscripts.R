library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(rstatix)
library(agricolae)
library(RColorBrewer)
#Dataset1
df <- read.csv ("Dataset1.csv", header=TRUE)
df$Time <- factor(df$Time, levels = c("Initial", "Final"))
res.aov2 <- aov(Glucose ~ Treatment + Time, data = df)
summary(res.aov2)
res.aov3 <- aov(Glucose ~ Treatment + Time + Treatment:Time, data = df)
summary(res.aov3)
TukeyHSD(res.aov3, which = "Treatment")
TukeyHSD(res.aov3, which = "Time")
df.summary1 <- df %>% group_by(Treatment, Time) %>% summarise(sd = sd(Glucose, na.rm = TRUE), se = sd/sqrt(4), Glucose = mean(Glucose))
Fig1_line <- ggplot(df.summary1, aes(x=Treatment, y=Glucose, group=Time)) + geom_line(aes(color=Time), size=1, alpha=0.9, linetype=1) + geom_point(aes(color=Time)) + geom_errorbar(aes(ymax = Glucose+se, ymin = Glucose-se), data = df.summary1, size = 0.5, width = 0.10) + theme_bw(base_size = 12, base_line_size = 1.0) + theme (axis.text.x = element_text(size=12, face = "bold", angle = 45, hjust = 1)) + theme (axis.text.y = element_text(size = 12)) + theme(text = element_text(size = 12, face = "bold")) + theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) + scale_y_continuous(expand = c(0,0)) + coord_cartesian(ylim = c(0, 25)) + scale_color_manual(values = c("#113A5C", "#B15928")) + labs(x="Treatment", y = "Blood glucose (mmol/L)")
Fig1_line
Fig1_col <- ggplot(df.summary1, aes(x=Treatment, y=Glucose, fill=Time)) + geom_col(position=position_dodge(), data = df.summary1, width = 0.7, color="black") + geom_errorbar(aes(ymax = Glucose+se, ymin = Glucose-se), data = df.summary1, size = 0.7, width = 0.3, position = position_dodge(0.7)) + theme_bw(base_size = 12, base_line_size = 1.0) + theme (axis.text.x = element_text(size=12, face = "bold", angle = 45, hjust = 1)) + theme (axis.text.y = element_text(size = 12)) + theme(text = element_text(size = 12, face = "bold")) + theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) + scale_y_continuous(expand = c(0,0)) + coord_cartesian(ylim = c(0, 25)) + scale_fill_manual(values = c("#113A5C", "#B15928")) + labs(x="Treatment", y = "Blood glucose (mmol/L)") 
Fig1_col
#Dataset2
df2 <- read.csv ("Dataset2.csv", header=TRUE)
df2$Day <- factor(df2$Day, levels = c("D0", "D7", "D14", "D21"))
res.aov2 <- aov(Weight ~ Treatment + Day, data = df2)
summary(res.aov2)
res.aov3 <- aov(Weight ~ Treatment + Day + Treatment:Day, data = df2)
summary(res.aov3)
df.summary1 <- df2 %>% group_by(Treatment, Day) %>% summarise(sd = sd(Weight, na.rm = TRUE), se = sd/sqrt(4), Weight = mean(Weight))
Fig2_line <- ggplot(df.summary1, aes(x=Treatment, y=Weight, group=Day)) + geom_line(aes(color=Day), size=1, alpha=0.9, linetype=1) + geom_point(aes(color=Day)) + geom_errorbar(aes(ymax = Weight+se, ymin = Weight-se), data = df.summary1, size = 0.5, width = 0.10) + theme_bw(base_size = 12, base_line_size = 1.0) + theme (axis.text.x = element_text(size=12, face = "bold", angle = 45, hjust = 1)) + theme (axis.text.y = element_text(size = 12)) + theme(text = element_text(size = 12, face = "bold")) + theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) + scale_y_continuous(expand = c(0,0)) + coord_cartesian(ylim = c(0, 50)) + scale_color_manual(values = c("#113A5C", "#B15928", "#458B74", "#CD9B1D")) + labs(x="Treatment", y = "Weight (g)")
Fig2_line
Fig2_col <- ggplot(df.summary1, aes(x=Treatment, y=Weight, fill=Day)) + geom_col(position=position_dodge(), data = df.summary1, width = 0.7, color="black") + geom_errorbar(aes(ymax = Weight+se, ymin = Weight-se), data = df.summary1, size = 0.7, width = 0.3, position = position_dodge(0.7)) + theme_bw(base_size = 12, base_line_size = 1.0) + theme (axis.text.x = element_text(size=12, face = "bold", angle = 45, hjust = 1)) + theme (axis.text.y = element_text(size = 12)) + theme(text = element_text(size = 12, face = "bold")) + theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = "white")) + scale_y_continuous(expand = c(0,0)) + coord_cartesian(ylim = c(0, 50)) + scale_fill_manual(values = c("#113A5C", "#B15928", "#458B74", "#CD9B1D")) + labs(x="Treatment", y = "Weight (g)") 
Fig2_col
plot_grid(Fig1_col, Fig2_col, labels = "AUTO", nrow = 1, rel_widths = c(1.6, 2.0))
