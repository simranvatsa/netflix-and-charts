library(tidyverse)
library(zoo)

netflix <- read_csv("NetflixViewingHistory.csv")
netflix$Date <- as.Date(netflix$Date, format = "%m/%d/%y")
netflix$Media_Type <- ifelse(str_count(netflix$Title, ":") >= 2, "TV", "movies and specials")
netflix$Date <- as.Date(netflix$Date, "%m/%d/%y")
netflix$Month_Year <- factor(as.yearmon(netflix$Date))
netflix$Year <- str_sub(as.character(netflix$Month_Year), 5, 8) %>% as.numeric()
netflix$`Show Title` <- ifelse(netflix$Media_Type == "TV", 
                               word(netflix$Title, sep = ":"), "Movie/Special")

shows_to_include2018 <- netflix %>% 
  filter(Year == "2018") %>% count(`Show Title`) %>% filter(n > 2)
shows18 <- netflix %>% 
  filter(`Show Title` %in% shows_to_include2018$`Show Title`, 
         Year == "2018", `Show Title` != "Movie/Special") 

p <- shows18 %>% ggplot() +
  geom_dotplot(aes(x = Month_Year, fill = `Show Title`), 
               stackratio = 1.4, dotsize = 0.65, binpositions = "all", 
               stackgroups = T) +
  scale_fill_manual(values = c("#CD71A0", "#3F6280", "#9FA7E7", 
                               "#FFBBD4", "#F1949E", "#0092C5", 
                               "#a8dff0", "#846F9A", "#B0416C", 
                               "#742554", "#70ffd9", "#ff02ab", "#1D0172",
                               "#8b3a62")) +
  ggtitle("Shows I watched on Netflix, by month, in 2018", 
          subtitle = "Each dot is one episode.") +
  labs(fill = "Show title") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        text = element_text(family = "Open Sans", size = 14), 
        axis.text = element_text(angle = 45), 
        plot.title = element_text(margin = margin(b = 10, t = 10), size = 22, 
                                  family = "Open Sans"),
        plot.subtitle = element_text(margin = margin(b = 5), size = 14, 
                                     family = "Open Sans"), 
        axis.title.x = element_blank())

p + 
  annotate("text", x = 7.9, y = 0.81, 
           label = "binged \n Insatiable -\n not my \n proudest \n moment", angle = 15, 
           family = "Nothing You Could Do", lineheight = 0.8, size = 4) + 
  
  annotate("segment", x = 0.8, xend = 4.2, y = 0.57, yend = 0.57, colour = "black") +
  annotate("segment", x = 0.8, xend = 0.8, y = 0.57, yend = 0.56, colour = "black") +
  annotate("segment", x = 4.2, xend = 4.2, y = 0.57, yend = 0.56, colour = "black") +
  
  annotate("text", x = 2.5, y = 0.61, label = "i barely\nwatched TV my\njunior year", 
           family = "Nothing You Could Do", lineheight = 0.8, size = 4) +
  annotate("text", x = 3.9, y = 0.35, 
           label = "finals in June - \n proof that \n the more \n stressed i am, 
           the more \n Gilmore Girls \n i watch", angle = 15, 
           family = "Nothing You Could Do", lineheight = 0.8, size = 4) + 
  annotate("text", x = 7.9, y = 0.265, label = "i discover \n Crazy Ex-\nGirlfriend", 
           angle = 15, family = "Nothing You Could Do", lineheight = 0.8, size = 4) + 
  
  annotate("segment", x = 8.8, xend = 11.2, y = 0.87, yend = 0.87, colour = "black") +
  annotate("segment", x = 8.8, xend = 8.8, y = 0.87, yend = 0.86, colour = "black") +
  annotate("segment", x = 11.2, xend = 11.2, y = 0.87, yend = 0.86, colour = "black") +
  
  annotate("text", x = 10, y = 0.96, label = "i've been \nwatching a wider
           variety of TV\n since October, \n& much more\nthan i did
           during the last\nschool year", family = "Nothing You Could Do", 
           lineheight = 0.8, size = 4) +
  
  annotate("text", x = 10.95, y = 0.51, label = "binged You \n - again, \nnot proud", 
           family = "Nothing You Could Do", lineheight = 0.8, size = 4, angle = 15) +
  annotate("text", x = 0.6, y = 1.025, label = "some shows i watched elsewhere:
           • The Marvelous Mrs. Maisel\n• The Good Place\n• Jane the Virgin
           • Brooklyn Nine-Nine\n• more Crazy Ex-Girlfriend (watched a
           whole season on a plane in September!)", 
           family = "Nothing You Could Do", lineheight = 0.9, 
           size = 4.5, hjust = 0, vjust = 0.8)