## Filename: obesity_paper_plot.R
## Author: Karthik Muthukumar

## Loading required libraries
library(ggplot2)
library(ggpubr)

## Change dataset file as per requirement
data_110_130<-read.csv("~/Desktop/R&D/Obesity Paper/09.18.2020_120day_data.csv",
                       row.names = 1, check.names = F)  

## Demographics Plot 

# Gender Distribution
fig1<-ggplot(data_110_130 , aes(x = 2,fill = factor(gender, levels = c("Female", "Male", "Decline to State"))))+
  geom_bar(stat = "count", width = 0.5, position = "stack", alpha = 0.8)+
  geom_text(aes(label=..count..),stat="count", size = 7, color ='black', position = position_stack(0.6))+
  coord_polar(theta = "y",start = 0)+
  theme_bw()+
  labs(x= NULL,y = NULL, fill= NULL, title = "Figure 1:" ,
       subtitle = "Distribution of participants across Gender")+
  scale_fill_manual(values=c("pink", "royalblue2","#D1D0CE"))+
  theme(text = element_text(size=20), axis.text.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.85, 0.93))+
  xlim(0.5, 2.5) +
  annotate(geom = 'text', x = 0.5, y = 0, 
           label = str_wrap(paste0("Total Participants = ",length(data_110_130$gender)), width = 20),
           size= 8)

# Weight Cohort Distribution Post 120 days
fig2<-ggplot(data_110_130, aes(x = 2,fill = factor(weight_cohort1,levels = c("Gained Weight", "Stable","Lost Weight"))))+
  geom_bar(stat = "count", width = 0.5, position = "stack", alpha = 1)+
  geom_text(aes(label=..count..),stat="count", size = 7, color ='black', position = position_stack(0.5))+
  xlim(0.5, 2.5)+
  coord_polar(theta = "y")+
  theme_bw()+
  labs(x= NULL,y = NULL, fill= NULL, title = "Figure 2:", subtitle ="Distribution of participants post 120 days in the program")+
  scale_fill_brewer(palette = "Pastel1")+
  theme(text = element_text(size=20), axis.text.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.84, 0.9))

ggarrange(fig1,fig2,nrow = 1)

# Dot plot - Start_weight vs Height
fig3<-ggplot(data_110_130, aes(height, start_wt,color = factor(bmi_start_cohort,levels = c("Underweight","Normal","Overweight",
                                                                                           "Class I", "Class II","Class III"))))+
  geom_point()+
  theme_bw()+
  labs(x= "Height (Inches)", y = "Start Weight (lbs)", color= "BMI Category", title = "Figure 3:")+
  scale_color_manual(values=c("#4FFF33","#FFB833","#3396FF","#E033FF", "#FF4933"))+
  theme(text = element_text(size=20),legend.background = element_rect(fill="lightblue",
                                                                      size=0.5, linetype="solid", 
                                                                      colour ="black"))+
  ylim(80,500)

# Dot plot - Last_weight vs Height
fig4<-ggplot(data_110_130, aes(height, data_110_130[,6],
                               color = factor(data_110_130[,17],levels = c("Normal","Overweight","Class I", "Class II", 
                                                                           "Class III"))))+
  geom_point()+
  theme_bw()+
  labs(x= "Height (Inches)", y = "Current Weight (lbs)", color= "BMI Category", title = "Figure 4:")+
  scale_color_manual(values=c("#4FFF33","#FFB833","#3396FF","#E033FF", "#FF4933"))+
  theme(text = element_text(size=20),legend.background = element_rect(fill="lightblue",
                                                                      size=0.5, linetype="solid", 
                                                                      colour ="black"))+
  ylim(80,500)

ggarrange(fig3,fig4,nrow = 1, common.legend = TRUE, legend = "bottom")


# BMI Swift 
fig12<-ggplot(data_110_130, aes(x= 2,fill = factor(bmi_swift_cohort, levels = c("Negative swift","No change","Positive swift"))))+
  geom_bar(color = 'black', stat = "count", width = 0.6, position = "stack")+
  geom_text(aes(label=..count..),stat="count", size = 7, color ='black', position = position_stack(0.6))+
  xlim(0.5, 2.5)+
  coord_polar(theta = "y")+
  theme_bw()+
  labs(x= NULL,y = NULL, fill= NULL, title = "Figure 12:",
       subtitle ="BMI shift of participants in the program post 120 days")+
  scale_fill_brewer(palette = "Pastel1")+
  theme(text = element_text(size=20), axis.text.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.84, 0.9))

# BMI Swift based on Start BMI Class
fig13<-ggplot(data_110_130, aes(factor(bmi_swift_cohort, levels = c("Negative swift","No change","Positive swift")),
                                fill = factor(bmi_start_cohort,levels = c("Normal","Overweight","Class I", "Class II", 
                                                                          "Class III"))))+
  geom_bar(color = 'black', stat = "count", width = 0.8, alpha = 0.74,position = "dodge")+
  geom_text(aes(label=..count..),stat="count", size = 6.5, color ='black', position = position_dodge(0.8), vjust = -.3)+
  theme_bw()+
  scale_fill_manual(values=c("#4FFF33","#FFB833","#3396FF","#E033FF", "#FF4933"))+
  labs(x= "",y = "Number of Participants", fill= "Start BMI", title = "Figure 13:",
       subtitle = str_wrap(paste0("BMI shift of participants post 120 days in the program based on start BMI Class"), width = 57))+
  theme(text = element_text(size=20),legend.background = element_rect(size=0.5, colour ="black"), 
        legend.position = c(0.84, 0.84))

ggarrange(fig12,fig13,nrow = 1, common.legend = F)


# Significant SNPs Risk Alleles distribution across participants
ob_data<-data_110_130%>% 
  select(dna_kit_id, 24:32)%>%## Select trait(s) names based on requirement
  tidyr::gather(.,key = trait, value = risk, -"dna_kit_id")

obesity_risk_count <- ob_data %>%  
  group_by(trait) %>%
  count(risk) %>%
  mutate(proportion = n/sum(n))%>%
  replace_na(list(risk = "Missing information"))

ggplot(obesity_risk_count,aes(x = trait, y = n,
                              fill = as.factor(risk))) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
  geom_text(aes(label = n, 
                y = n, 
                group = as.factor(risk)),
            position = position_stack(0.6),
            size = 3.5)+
  theme_bw()+
  theme(text = element_text(size=17),
        axis.text.y = element_text(angle = 0, size = 12, face = "bold"),
        axis.text.x = element_text(angle = 0, size = 9, face = "bold"),
        legend.position = "bottom",legend.background = element_rect(size=0.5, colour ="black"))+
  labs(x = "", y = "Number of Patients (%)", title = "Figure 16: Distribution of Patients across Significant snps" ,
       fill = str_wrap("Number of Risk Alleles Present",width = 20))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  scale_fill_manual(values=c("#4FFF33","#FFB833", "#FF4933", "grey"))

