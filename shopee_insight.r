setwd("C:/Users/ThisVraPxng/Desktop/")
#install.packages("readxl")
library(readxl)
library(ggplot2)
library(dplyr)
library(ggwordcloud)

df <- read_excel("C:/Users/ThisVraPxng/Desktop/Infographic_6233200631.xlsx")
df
table(df$type_of_product)
class(t)

#Create Sumarize

type <- df %>% count(type_of_product)
                    
##bar plot
ggplot(type,aes(reorder(type_of_product,n),n)) +
  geom_col(fill='darkorange2',col='darkorange4') +
  labs(x='Type_Product',y='Total_of_product',title='Number of Product to sale on Shopee Home-Appliances',subtitle='Data from Shopee 12/10/21')+ theme_classic() +theme(plot.background = element_rect(fill = "darkgoldenrod2"))+geom_text(aes(label = n), hjust = 1.5)+
  coord_flip()

#create data frame all sell
df$total_all_sales <- (df$total_sale)*(df$sale_price)
table(df$type_of_product)

##################################
df1 <- df %>%
  group_by(type_of_product) %>%
  summarize(total_all_sales = sum(total_all_sales))

ggplot(df1,aes(reorder(type_of_product,total_all_sales),total_all_sales)) +
  geom_col(fill='chocolate',col='darkorange4') +
  labs(x='Type_Product',y='total_all_sales',title='Sales that can be sold in each category on Shopee Home-Appliances')+ theme_classic() +theme(plot.background = element_rect(fill = "chocolate1"))+
  geom_text(aes(label = total_all_sales), hjust = 1.5)+
  coord_flip()
########################################################
df4 <- df %>%
  group_by(preferredStore_tag) %>%
  summarize(total_sale = sum(total_all_sales))



####################count location of store
#install.packages("ggwordcloud")
df5 <- df %>% count(location_store)
#Plot (Word Cloud) 
options(repr.plot.width = 20, repr.plot.height = 15)
ggplot(df5,aes(label = location_store,
               size = n,
               col = location_store)) + geom_text_wordcloud()+ theme_classic() +
  scale_size_area(max_size = 28)+
  labs(title = "Location of the store ")
################DF6####
######################compare data about discount
df$percent_discount[is.na(df$percent_discount)] = 0
df6 <- df %>%
  group_by(percent_discount > 50,percent_discount != 0) %>%
  summarize(total_sale = sum(total_all_sales))
df6$data <- c("No_Discount","Discount<50","Discount>50")
###############################################################

piepercent<- round(100*df6$total_sale/sum(df6$total_sale), 1)
pie(df6$total_sale, labels = piepercent, main = "When a product is discounted, there will be a sold.",col = heat.colors(length(df6$total_sale),alpha = 0.8))
legend("topright", df6$data, cex = 0.8,fill = heat.colors(length(df6$total_sale),alpha = 0.8))
###################################################################
min(df$sale_price);max(df$sale_price)

ggplot(df, aes(x=sale_price, y=type_of_product)) +
  geom_point(alpha = .5, fill="cornflowerblue", color="blue", shape=21)
####
ggplot(data=df, aes(x=type_of_product, y=price_range, fill=price_range)) +
  geom_bar(stat="identity")+ theme_classic() +theme(plot.background = element_rect(fill = "orange2"))+
  coord_flip()

