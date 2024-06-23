library(dplyr)
library(seqinr)
library(tidyr)
library(ggplot2)
library(readxl)
library(egg)

total <- read_xlsx("sea_level.xlsx", sheet=2)
sterodynamic <- read_xlsx("sea_level.xlsx", sheet=3)
GIS <- read_xlsx("sea_level.xlsx", sheet=4)
AIS <- read_xlsx("sea_level.xlsx", sheet=5)
glaciers <- read_xlsx("sea_level.xlsx", sheet=6)
VLM <- read_xlsx("sea_level.xlsx", sheet=7)
LWS <- read_xlsx("sea_level.xlsx", sheet=8)

total_ssp4570 <- (total[c(12,13,14), 6:19]+total[c(17,18,19), 6:19])/2

total_ssp585 <- total[c(22,23,24), 6:19]
total_ssp585 <- as.data.frame(t(total_ssp585))
colnames(total_ssp585) <- c('low','level', 'high')


sterodynamic_4570 <- (sterodynamic[c(12,13,14), 6:19]+sterodynamic[c(17,18,19), 6:19])/2
sterodynamic_4570 <- as.data.frame(t(sterodynamic_4570))
colnames(sterodynamic_4570) <- c('low','level', 'high')

sterodynamic_585 <- sterodynamic[c(22,23,24), 6:19]
sterodynamic_585 <- as.data.frame(t(sterodynamic_585))
colnames(sterodynamic_585) <- c('low','level', 'high')


GIS4570 <- (GIS[c(12,13,14), 6:19]+GIS[c(17,18,19), 6:19])/2
GIS4570 <- as.data.frame(t(GIS4570))
colnames(GIS4570) <- c('low','level', 'high')
GIS585 <- GIS[c(22,23,24), 6:19]
GIS585 <- as.data.frame(t(GIS585))
colnames(GIS585) <- c('low','level', 'high')

AIS4570 <- (AIS[c(12,13,14), 6:19]+AIS[c(17,18,19), 6:19])/2
AIS4570 <- as.data.frame(t(AIS4570))
colnames(AIS4570) <- c('low','level', 'high')
AIS585 <- AIS[c(22,23,24), 6:19]
AIS585 <- as.data.frame(t(AIS585))
colnames(AIS585) <- c('low','level', 'high')


glaciers4570 <- (glaciers[c(12,13,14), 6:19]+glaciers[c(17,18,19), 6:19])/2
glaciers4570 <- as.data.frame(t(glaciers4570))
colnames(glaciers4570) <- c('low','level', 'high')
glaciers585 <- glaciers[c(22,23,24), 6:19]
glaciers585 <- as.data.frame(t(glaciers585))
colnames(glaciers585) <- c('low','level', 'high')

VLM4570 <-(VLM[c(12,13,14), 6:19]+VLM[c(17,18,19), 6:19])/2
VLM4570 <- as.data.frame(t(VLM4570))
colnames(VLM4570) <- c('low','level', 'high')
VLM585 <- VLM[c(22,23,24), 6:19]
VLM585 <- as.data.frame(t(VLM585))
colnames(VLM585) <- c('low','level', 'high')

LWS4570 <-(LWS[c(12,13,14), 6:19]+LWS[c(17,18,19), 6:19])/2
LWS4570 <- as.data.frame(t(LWS4570))
colnames(LWS4570) <- c('low','level', 'high')
LWS585 <- LWS[c(22,23,24), 6:19]
LWS585 <- as.data.frame(t(LWS585))
colnames(LWS585) <- c('low','level', 'high')

total_ssp4570 <- as.data.frame(t(total_ssp4570))
colnames(total_ssp4570) <- c('low4570','level4570', 'high4570')


colors <- c("Average SSP2-4.5 and SSP3-7.0" = "blue", "SSP5-8.5" = "red")

ggplot(total_ssp4570, aes(x = as.numeric(rownames(total_ssp4570)))) +
  geom_line(aes(y = level4570, color = "Average SSP2-4.5 and SSP3-7.0"))+
  geom_ribbon(aes(ymin = low4570, ymax = high4570), alpha = 0.1, fill="blue")+
  xlab('year')+ylab('RLSC (m)')+
  geom_line(data = total_ssp585, aes(y=level, color = "SSP5-8.5")) + 
  geom_ribbon(data = total_ssp585, aes(ymin = low, ymax = high), alpha = 0.1, fill="red")+
  labs(title = "Total Relative Sea Level Rise",
       color = "Scenario's")+
  scale_color_manual(values = colors)+
  theme_bw()+ theme(legend.position = c(0.1, 0.9),
                    legend.background = element_blank(),
                    legend.box.background = element_rect(colour = "black"))+ylim(-0.5,2) +
  geom_point(data=total_ssp4570, aes(x=2100, y=level4570[9]))+
  geom_text(data = total_ssp4570,
            aes(x = 2100,
                y = level4570[9],
                label = level4570[9]),
            vjust = 1.5)+
  geom_point(data=total_ssp585, aes(x=2100, y=level[9]))+
  geom_text(data = total_ssp585,
            aes(x = 2100,
                y = level[9],
                label = level[9]),
            vjust = -1)



sterodynamic <- ggplot(sterodynamic_4570, aes(x = as.numeric(rownames(total_ssp4570)))) +
  geom_line(aes(y = level, color = "Average SSP2-4.5 and SSP3-7.0"))+
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1, fill="blue")+
  xlab('')+ylab('RLSC (m)')+
  geom_line(data = sterodynamic_585, aes(y=level, color = "SSP5-8.5")) + 
  geom_ribbon(data = sterodynamic_585, aes(ymin = low, ymax = high), alpha = 0.1, fill="red")+
  labs(title = "Sterodynamic",
       color = "Scenario's")+
  scale_color_manual(values = colors)+
  theme_bw()+ theme(axis.text.x=element_blank(),
                    legend.position = c(0.2, 0.8),
                    legend.background = element_blank(),
                    legend.box.background = element_rect(colour = "black")) + ylim(-0.5,1.2) +
  geom_point(data=sterodynamic_4570, aes(x=2100, y=level[9]))+
  geom_text(data = sterodynamic_4570,
            aes(x = 2100,
                y = level[9],
                label = level[9]),
            vjust = 1.3)+
  geom_point(data=sterodynamic_585, aes(x=2100, y=level[9]))+
  geom_text(data = sterodynamic_585,
            aes(x = 2100,
                y = level[9],
                label = level[9]),
            vjust = -1)


GIS <- ggplot(GIS4570, aes(x = as.numeric(rownames(GIS4570)))) +
  geom_line(aes(y = level, color = "Average SSP2-4.5 and SSP3-7.0"))+
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1, fill="blue")+
  xlab('')+ylab('')+
  geom_line(data = GIS585, aes(y=level, color = "SSP5-8.5")) + 
  geom_ribbon(data = GIS585, aes(ymin = low, ymax = high), alpha = 0.1, fill="red")+
  labs(title = "GIS",
       color = "Scenario's")+
  scale_color_manual(values = colors)+
  theme_bw()+ theme(axis.text.x=element_blank(),
                    legend.position = c(0.2, 0.8),
                    legend.background = element_blank(),
                    legend.box.background = element_rect(colour = "black"))+ylim(-0.5,1.2) +
  geom_point(data=GIS4570, aes(x=2100, y=level[9]))+
  geom_text(data = GIS4570,
            aes(x = 2100,
                y = level[9],
                label = level[9]),
            vjust = 1.3)+
  geom_point(data=GIS585, aes(x=2100, y=level[9]))+
  geom_text(data = GIS585,
            aes(x = 2100,
                y = level[9],
                label = level[9]),
            vjust = -1)





AIS <- ggplot(AIS4570, aes(x = as.numeric(rownames(AIS4570)))) +
  geom_line(aes(y = level, color = "Average SSP2-4.5 and SSP3-7.0"))+
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1, fill="blue")+
  xlab('')+ylab('')+
  geom_line(data = AIS585, aes(y=level, color = "SSP5-8.5")) + 
  geom_ribbon(data = AIS585, aes(ymin = low, ymax = high), alpha = 0.1, fill="red")+
  labs(title = "AIS",
       color = "Scenario's")+
  scale_color_manual(values = colors)+
  theme_bw()+ theme(axis.text.x=element_blank(),
                    legend.position = c(0.2, 0.8),
                    legend.background = element_blank(),
                    legend.box.background = element_rect(colour = "black"))+ ylim(-0.5,1.2) +
  geom_point(data=AIS4570, aes(x=2100, y=level[9]))+
  geom_text(data = AIS4570,
            aes(x = 2100,
                y = level[9],
                label = level[9]),
            vjust = 1.3)+
  geom_point(data=AIS585, aes(x=2100, y=level[9]))+
  geom_text(data = AIS585,
            aes(x = 2100,
                y = level[9],
                label = level[9]),
            vjust = -1)


glaciers <- ggplot(glaciers4570, aes(x = as.numeric(rownames(glaciers4570)))) +
  geom_line(aes(y = level, color = "Average SSP2-4.5 and SSP3-7.0"))+
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1, fill="blue")+
  xlab('')+ylab('RLSC (m)')+
  geom_line(data = glaciers585, aes(y=level, color = "SSP5-8.5")) + 
  geom_ribbon(data = glaciers585, aes(ymin = low, ymax = high), alpha = 0.1, fill="red")+
  labs(title = "Glaciers",
       color = "Scenario's")+
  scale_color_manual(values = colors)+
  theme_bw()+ theme(legend.position = c(0.2, 0.8),
                    legend.background = element_blank(),
                    legend.box.background = element_rect(colour = "black"))+ ylim(-0.5,1.2) +
  geom_point(data=glaciers4570, aes(x=2100, y=level[9]))+
  geom_text(data = glaciers4570,
            aes(x = 2100,
                y = level[9],
                label = level[9]),
            vjust = 1.3)+
  geom_point(data=glaciers585, aes(x=2100, y=level[9]))+
  geom_text(data = glaciers585,
            aes(x = 2100,
                y = level[9],
                label = level[9]),
            vjust = -1)



VLM <- ggplot(VLM4570, aes(x = as.numeric(rownames(VLM4570)))) +
  geom_line(aes(y = level, color = "Average SSP2-4.5 and SSP3-7.0"))+
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1, fill="blue")+
  xlab('year')+ylab('')+
  geom_line(data = VLM585, aes(y=level, color = "SSP5-8.5")) + 
  geom_ribbon(data = VLM585, aes(ymin = low, ymax = high), alpha = 0.1, fill="red")+
  labs(title = "Vertical Land Motion",
       color = "Scenario's")+
  scale_color_manual(values = colors)+
  theme_bw()+ theme(legend.position = c(0.2, 0.8),
                    legend.background = element_blank(),
                    legend.box.background = element_rect(colour = "black"))+ ylim(-0.5,1.2) +
  geom_point(data=VLM4570, aes(x=2100, y=level[9]))+
  geom_text(data = VLM4570,
            aes(x = 2100,
                y = level[9],
                label = level[9]),
            vjust = 1.3)+
  geom_point(data=VLM585, aes(x=2100, y=level[9]))+
  geom_text(data = VLM585,
            aes(x = 2100,
                y = level[9],
                label = level[9]),
            vjust = -1)


LWS <- ggplot(LWS4570, aes(x = as.numeric(rownames(LWS4570)))) +
  geom_line(aes(y = level, color = "Average SSP2-4.5 and SSP3-7.0"))+
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1, fill="blue")+
  xlab('')+ylab('')+
  geom_line(data = LWS585, aes(y=level, color = "SSP5-8.5")) + 
  geom_ribbon(data = LWS585, aes(ymin = low, ymax = high), alpha = 0.1, fill="red")+
  labs(title = "Land Water Storage",
       color = "Scenario's")+
  scale_color_manual(values = colors)+
  theme_bw()+ theme(legend.position = c(0.2, 0.8),
                    legend.background = element_blank(),
                    legend.box.background = element_rect(colour = "black"))+ ylim(-0.5,1.2) +
  geom_point(data=LWS4570, aes(x=2100, y=level[9]))+
  geom_text(data = LWS4570,
            aes(x = 2100,
                y = level[9],
                label = level[9]),
            vjust = 1.3)+
  geom_point(data=LWS585, aes(x=2100, y=level[9]))+
  geom_text(data = LWS585,
            aes(x = 2100,
                y = level[9],
                label = level[9]),
            vjust = -1)


figure <- ggarrange(sterodynamic, GIS, AIS, glaciers, VLM, LWS,
                    ncol = 3, nrow = 2)
