library("tidyverse")
library("ggExtra")
library("GGally")
library("ggthemes")
library("ggpubr")
library("reshape2")
library("magrittr")
library(corrplot)
library("ggcorrplot")
df10 <- read_csv(file = "dataset-of-10s.csv")
# Define discrete as factors

uniqueValuesPColumns <- df10 %>%
  mutate(across(everything(), ~ length(unique(.x)))) %>%
  head(n = 1) %>%
  pivot_longer(cols = everything(),names_to = "Column",
               values_to = "numberUnique") %>%
  arrange(numberUnique)


df10 %<>%
  mutate(duration_s= duration_ms / 1000 )
# It looks as though we need to set a few of these as factors
df10 %<>%
  mutate(across(c(mode,key,time_signature), ~ factor(.x)))
df10 %<>%
  select(-track,-artist,-uri,-target,-duration_ms)
df10 %>% 
  select(where(is.factor))

SpotifyGreen <- "#1DB954"

df10 %>%
  ggplot(aes(danceability)) +
  geom_density()
df10 %>%
  ggplot(aes(chorus_hit)) +
  geom_density()
df10 %>%
  select(where(is.numeric)) %>%
  cor(method = "spearman") %>%
  arm::corrplot(method="color",col.lim =   
             c(-1,1),cex.col = 1.2,col=colorRampPalette(c("black","grey",SpotifyGreen))(200))
df10 %>%
  select(where(is.numeric)) %>%
  cor(method = "spearman") %>%
  ggcorrplot(colors = c("Black","Grey",SpotifyGreen)) + 
  scale_x_discrete(labels = c("Sections in Song", "Time Until Chorus Hit", "Duration of Song in Seconds", "Tempo", 
                              "Valence", "Liveness", "Instrumentalness","Acousticness","Speechiness","Loudness",
                              "Energy","Danceability")) + 
  scale_y_discrete(labels = c("Sections in Song", "Time Until Chorus Hit", "Duration of Song in Seconds", "Tempo", 
                              "Valence", "Liveness", "Instrumentalness","Acousticness","Speechiness","Loudness",
                              "Energy","Danceability"))
numeric_labels <- c(`sections` = "Sections in Song", `chorus_hit` = "Time Until Chorus Hit"
                    ,`duration_s` = "Duration of Song in Seconds",`tempo` =  "Tempo", 
  `valence` = "Valence",`liveness` =  "Liveness",  `instrumentalness` = "Instrumentalness",
  `acousticness` = "Acousticness", `speechiness` = "Speechiness", `loudness` = "Loudness",
  `energy` = "Energy", `danceability` = "Danceability")

df10 %<>%
  mutate(duration_s= duration_ms / 1000 )

df10 %>%
  select(where(is.numeric)) %>%
  gather() %>% 
  ggplot(aes(value)) + 
  geom_histogram(fill = SpotifyGreen) + 
  ylab("Frequency") +
  facet_wrap(~key, scales = 'free',labeller = as_labeller(numeric_labels)) 

df10 %>%
  select(where(is.numeric)) %>%
  gather() %>% 
  ggplot(aes(value)) + 
  geom_histogram(aes(y=..count../sum(..count..)),fill = SpotifyGreen) + 
  ylab("Relative Frequency") +
  facet_wrap(~key, scales = 'free',labeller = as_labeller(numeric_labels)) 

 + theme(axis.text=element_text(size=12),
      axis.title=element_text(size=14,face="bold"))
# Let's do the same thing for  factors
factorLabels <- c(`mode` = "Mode",`key` = "Key")

df10 %>%
  select(where(is.factor)) %>%
  gather() %>% 
  ggplot(aes(value)) + 
  geom_bar(fill = SpotifyGreen) + 
  facet_wrap(~key, scales = 'free')
par(mfrow = c(2,1))

df10 %>% ggplot(aes(x = mode)) +
  geom_bar(fill = SpotifyGreen) +
  xlab("Mode")
df10 %>% ggplot(aes(x = fct_infreq(key))) +
  geom_bar(fill = SpotifyGreen) +
  xlab("Key")
df10 %>% 
  ggplot(aes(x = time_signature)) +
  geom_bar(fill  = SpotifyGreen) +
  xlab("Time Signature")


df10 %>% 
  select(mode,key) 
  gather() %>% 
  ggplot(aes(value)) + 
  geom_bar(fill = SpotifyGreen) + 
  facet_wrap(~key, scales = 'free')
  df10 %>% 
  ggplot(aes(key)) +
  geom_bar()
df10$key

df10 %>%
  ggplot(aes(danceability,energy)) +
  geom_jitter() +
  geom_smooth()
df10 %>%
  ggplot(aes(x = factor(key),fill = factor(key))) +
  geom_bar()
df10 %>%
  ggplot(aes(x = factor(mode))) +
  geom_bar()
df10 %>%
  ggplot(aes(x = log(acousticness), y = energy)) +
  geom_point() +
  geom_smooth(method = "lm")

# These plots seems to be pretty informative

df10 %>%
  select(where(is.numeric)) %>%
  cor() %>%
  corrplot::corrplot()

LDPlot <- df10 %>%
  ggplot(aes(x = loudness, y = danceability)) +
  geom_point(color = SpotifyGreen) +
  geom_smooth(method = "lm",color = "black") +
  theme_tufte()
ggMarginal(LDPlot,type = "histogram")

ggMarginal(df10 %>%
  ggplot(aes(x = log(abs(loudness)), y = danceability)) +
  geom_point(color = SpotifyGreen) +
  geom_smooth(method = "lm",color = "black") +
  theme_tufte())

PEPlot <- df10 %>%
  ggplot(aes(x = loudness, y = sqrt(energy))) +
  geom_point(color = SpotifyGreen,size = .5,alpha = .2) +
  geom_smooth(method = "lm",color = "black") +
  xlab("Loudness") +
  ylab("Square Root of Energy") +
  theme_tufte()
ggMarginal(PEPlot,type = "histogram")

df10 %<>%
  mutate(lowSinging = factor(if_else(instrumentalness > .5,1,0)))

ILPlot <- df10 %>%
  mutate(lowSinging = factor(if_else(instrumentalness > .5,1,0))) %>%
  ggplot(aes(lowSinging, loudness)) +
  geom_boxplot(color = SpotifyGreen,fill = "Black") +
  theme_tufte()
ILPlot
VDPlot <- df10 %>%
  ggplot(aes(valence, danceability)) +
  geom_point(color = SpotifyGreen) +
  geom_smooth(method = "lm", color = "black") +
  xlab("Valence") +
  ylab("Danceability") +
  theme_tufte()
ggMarginal(VDPlot,type = "histogram")
# Log transform duration
# Change dot thickness!
SDPlot <- df10 %>%
  mutate(bin = factor(cut_interval(as.numeric(sections),length = 5))) %>%
  ggplot(aes(duration_ms,bin,color = bin)) +
  geom_boxplot() +
  ylab("Number of Sections") +
  xlab("duration of song in miliseconds") +
  theme_tufte()
SpDaPlot <- df10 %>%
  ggplot(aes(speechiness,danceability)) +
  geom_point(color = SpotifyGreen,size = .25) +
  theme_tufte()
ggMarginal(SpDaPlot,type = "histogram")

TLPlot <- df10 %>%
  ggplot(aes(tempo,loudness)) +
  geom_point(color = SpotifyGreen,size = .2) +
  theme_tufte()
ggMarginal(TLPlot,type = "histogram")

AEPlot <- df10 %>%
  ggplot(aes(x = sqrt(acousticness), y = energy)) +
  geom_point(color = SpotifyGreen,size = .25) +
  geom_smooth(method = "lm",color = "black",se = T) +
  stat_regline_equation(label.y = .12,label.x = .15, aes(label = ..rr.label..)) +
  theme_tufte() +
  xlab("Square Root of Acousticness")
ggMarginal(AEPlot,type = "histogram")

ChDPlot <- df10 %>%
  ggplot(aes(log(chorus_hit + 1),log(duration_ms))) +
  geom_point(color = SpotifyGreen,size = .2) +
  xlab("Log Time Until Chorus Hit in Seconds") +
  ylab("Log Duration of Time in Milliseconds") + 
  theme_tufte()
ggMarginal(ChDPlot,type = "histogram")

ggsave(ChDPlot,filename = "ChorusDurationPlot", device = "jpeg" )


LSPlot <- df10 %>% 
  ggplot(aes(liveness^.2,speechiness^.2)) +
  geom_point(color = SpotifyGreen,size = .6) +
  geom_smooth(method = "lm",color = "grey") +
  ylab("Fifth Root of Speechiness") +
  xlab("Fifth Root of Liveness") +
  theme_tufte()
ggMarginal(LSPlot,type = "histogram")

TCVPlot <- df10 %>% 
  ggplot(aes(x = chorus_hit,valence)) +
  geom_point(size = .6,color = SpotifyGreen) +
  geom_smooth(method = "lm",color = "gray") +
  xlab("Time Until Chorus Hit") +
  ylab("Valence") +
  theme_tufte()
ggMarginal(TCVPlot,type = "histogram")


mpg %>% 
  ggplot(aes(sample = cty)) +
  stat_qq(aes(color = class)) +
  stat_qq_line()
stat_
  
