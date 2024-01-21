setwd("C:/Users/ThisVraPxng/Desktop")
##install.packages("tidyr")
#install ggflag
devtools::install_github("ellisp/ggflags")
library(tidyr);library(dplyr);library(ggplot2);library(readxl);library(gganimate);library(lubridate);library(ggflags);library(countrycode);library(tidyverse);library(ggthemes);library(readr);library(magick)


#import data covid19 in USA
df <- read_excel("C:/Users/ThisVraPxng/Desktop/Animation_6233200631.xlsx")
##prepare data
data <- df %>%
  group_by(Year,Country_of_Residence) %>%
  filter(continent == "East Asia") %>%
  summarise(a=sum(TOTAL))
data$Year<- as.Date.factor(data$Year,format ="%Y")
reoder1 <-  df %>% arrange(desc(TOTAL))
reoder1
##reorder TOTAL Average
df11 <- df %>%
  group_by(Country_of_Residence) %>%
  filter(continent == "East Asia") %>%
  summarise(a=mean(TOTAL))
reoder <-  df11 %>% arrange(desc(a))
reoder
##average 2008-2019 TOTAL
##1 China                88616.
##2 Malaysia             27739.
##3 Korea                18276.
##4 Japan                15822.
##5 Singapore             8149.
##6 Vietnam               6408.

data1 <- df %>%
  group_by(Year,Country_of_Residence) %>%
  filter(continent == "East Asia") %>%
  filter(Country_of_Residence == "Malaysia"|Country_of_Residence == "Korea"|Country_of_Residence == "Japan"|
           Country_of_Residence == "Singapore"|Country_of_Residence == "Vietnam")%>%
  summarise(a=sum(TOTAL))
data1$Year<- as.Date.factor(data1$Year,format ="%Y")
data1$Year1<-format(as.Date(data1$Year, format="%Y/%m/%d"),"%Y")
class(data1$Year1)
data1$Code <- tolower(countrycode(data1$Country_of_Residence, "country.name", "iso2c"))
####plot geom_line
data1$a<-round(data1$a,digits = 0)
####################################create gif#################################################

plt <- ggplot(data1, aes(x=Year,y=a,country=Code,color=Country_of_Residence)) + geom_line(size = 1,alpha = 0.7) +
  geom_vline(aes(xintercept = Year))+geom_point()+geom_flag(size = 14)+
  xlab("Year") + ylab("Total revenue to Thailand")+
  theme(plot.title = element_text(size = 32,hjust = 0.5,face = "bold"))+
  ggtitle("5 countries that spend on tourism in Thailand
          (Unit : Million Baht)     continent: East Asia")+
  geom_text(aes(x=max(Year), label=sprintf("%1.0fk", a/1000)), 
            nudge_x=3,vjust= -1, hjust=1, show.legend=F, size=5.5)+
  transition_reveal(Year)+ theme_solarized_2(light = FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position=c(0.23,0.8))+                                                     
  geom_text(aes( y = 65, label = Year1), size = 9, show.legend = FALSE, color = "grey", alpha = 0.25)+
  theme(axis.line.x = element_blank(),axis.text.x = element_blank())
#plt
#####plot 30 frame

####################################################################################################################
eu <- df %>%
  group_by(Country_of_Residence) %>%
  filter(continent == "Europe") %>%
  summarise(a=mean(TOTAL))
data$Year<- as.Date.factor(data$Year,format ="%Y")
reoder2 <-  eu %>% arrange(desc(a))
reoder2
data2 <- df %>%
  group_by(Year,Country_of_Residence) %>%
  filter(continent == "Europe") %>%
  filter(Country_of_Residence == "Russia"|Country_of_Residence == "United Kingdom"|Country_of_Residence == "Germany"|
           Country_of_Residence == "France"|Country_of_Residence == "Sweden")%>%
  summarise(Shop=sum(Shopping),a=sum(TOTAL))
data2$Year<- as.Date.factor(data2$Year,format ="%Y")
data2$Year1<-format(as.Date(data2$Year, format="%Y/%m/%d"),"%Y")
data2$Code <- tolower(countrycode(data2$Country_of_Residence, "country.name", "iso2c"))
plt2 <- ggplot(data2, aes(x=Year,y=a,country=Code,color=Country_of_Residence)) + geom_line(size = 1,alpha = 0.7) +
  geom_vline(aes(xintercept = Year))+geom_point()+geom_flag(size = 14)+
  xlab("Year") + ylab("Total revenue to Thailand")+
  theme(plot.title = element_text(size = 32,hjust = 0.5,face = "bold"))+
  ggtitle("5 countries that spend on tourism in Thailand
          (Unit : Million Baht)      continent:Europe")+
  geom_text(aes(x=max(Year), label=sprintf("%1.0fk", a/1000)), 
            nudge_x=3,vjust= -1, hjust=1, show.legend=F, size=5.5)+
  transition_reveal(Year)+ theme_solarized_2(light = FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position=c(0.23,0.8))+                                                     
  geom_text(aes( y = 65, label = Year1), size = 9, show.legend = FALSE, color = "grey", alpha = 0.25)+
  theme(axis.line.x = element_blank(),axis.text.x = element_blank())
#plt2

##animate 2 graph
ani_plt <-  animate(plt,
                    nframes = nrow(data2),
                    width = 7, height = 7, units = "in", res = 155,
                    renderer = magick_renderer())
ani_plt2 <-  animate(plt2,
                    nframes = nrow(data2),
                    width = 7, height = 7, units = "in", res = 155,
                    renderer = magick_renderer())
#Merge 2 graph to 1 graph
new_gif <- image_append(c(ani_plt[1],ani_plt2[1]))
for(i in 2:nrow(data2)){
  combined <-image_append(c(ani_plt[i],ani_plt2[i]))
  new_gif<- c(new_gif,combined)
}
new_gif
#export gif file
anim_save(filename = "6233200631.gif", 
          animation = new_gif,
          path = "C:/Users/ThisVraPxng/Desktop",delay=25)
