################################################################################
# IMPORT AND AND MERGE DATA
setwd("C:/Users/gaoan/Desktop/steamspyanalyzer")
rm(list=ls())

df = read.csv("step12_result.csv")   

df$positivity = df$pos_rev_num / (df$pos_rev_num + df$neg_rev_num)

################################################################################
# CALCULATE NUMBER OF TAGS AT TIME 0

df$num_of_tags_t1 = rowSums(df[115:539]!=0)
# df$num_of_tags_t2 = rowSums(df[542:966]!=0)
df$total_tags_count_t2 = rowSums(df[542:966])

################################################################################
# CALCULATE AUDIENCE ENTROPY (using vector length = 425)

require("entropy")

ent_vec = vector()

for (i in 1: nrow(df)){
  
  g1 = df[i,542:966]
  
  df_t = as.data.frame(t(g1))
  colnames(df_t) = "count"
  df_t$count = as.integer(df_t$count)
  df_t$tag_names = rownames(df_t)
  
  
  freqs = freqs.empirical(df_t$count)
  ent_t2 = entropy.empirical(freqs, unit="log2")
  
  ent_vec = c(ent_vec, ent_t2)
  
}

df$entropy = ent_vec

################################################################################
# CALCULATE JACCARD SIMILARITY BETWEEN VECTORS OF PRODUCER AND AUDIENCE TAGS

jacc_vec = vector()

for (i in 1: nrow(df)){
  
  jaccard <- function(a, b) {
    intersection = length(intersect(a, b))
    union = length(a) + length(b) - intersection
    return (intersection/union)
  }
  
  # for (i in 1: nrow(df)){
  t1 = df[i,115:539]
  t2 = df[i,542:966]
  df_t1 = as.data.frame(t(t1))
  df_t2 = as.data.frame(t(t2))
  
  colnames(df_t1) = "count"
  colnames(df_t2) = "count"
  
  df_t1$tag_names = rownames(df_t1)
  df_t2$tag_names = rownames(df_t1)
  
  df_t1$count = as.integer(df_t1$count)
  df_t2$count = as.integer(df_t2$count)
  
  df_new = merge(df_t1,df_t2,by="tag_names")
  df_new = df_new[df_new$count.x != 0 | df_new$count.y != 0,]
  
  df_new$count.x[df_new$count.x >0] = 1
  df_new$count.y[df_new$count.y >0] = 1
  
  jacc = jaccard(df_new$count.x,df_new$count.y)
  
  jacc_vec = c(jacc_vec, jacc)
  
}

df$jacc = jacc_vec

################################################################################
# MERGE IN OTHER MEASURES AND CREATE SUBSET

df = df[,c('app_id', 'price_x', 'owners_x', 'genre_count',
           'num_of_tags_t1', 'total_tags_count_t2', 'jacc', 'entropy')]

df = df[df$genre_count!=0,]
df = df[!duplicated(df),]

typ = read.csv("typicality.csv")
colnames(typ)[1] = "app_id"
typ = typ[!duplicated(typ),]

data = merge(df,typ,by="app_id", all=F)

# cos_sim_df = read.csv("cos_sim.csv")                # MERGE THIS IN LATER FOR ROBUSTNESS CHECKS
# colnames(cos_sim_df)[1] = "app_id"
# cos_sim_df = cos_sim_df[!duplicated(cos_sim_df),]
# 
# data = merge(df,cos_sim_df,by="app_id", all=F)

franch = read.csv("step13_scraped_steamdb_data.csv")
franch = franch[!duplicated(franch),]
data = merge(data,franch,by="app_id", all=F)

data$franchise[is.na(data$franchise_name)] = 0
data$franchise[!is.na(data$franchise_name)] = 1

data$rating = as.numeric(gsub("%", "", data$steamdb_score))
data$franchise = as.factor(data$franchise)
data$app_id = as.integer(data$app_id)
data$owners_x = as.integer(data$owners_x)
data$price_x = as.integer(data$price_x)

################################################################################
# PRODUCE AND EXPORT DESCRIPTIVE STATISTICS

desc_df = data[,c('price_x', 'rating', 'franchise', 'owners_x', 'genre_count', 'TYPICALITY', 'jacc', 'entropy')]

# TABLE 1 PT. 1 -- MEAN AND SD


require(psych)
desc_df$franchise = as.integer(desc_df$franchise)
descriptives = describe(desc_df)

write.table(descriptives,file="./exports/descriptive_stats.csv", sep=",")

# TABLE 1 PT. 2 -- CORR MATRIX

library(Hmisc)                                                    #WHEN YOU LOAD THIS PSYCH::DESCRIBE() STOPS WORKING

desc_df$franchise = as.integer(desc_df$franchise)

rcx = rcorr(as.matrix(desc_df))
desc_df.rcx.r=data.frame(rcx$r)
desc_df.rcx.p=data.frame(rcx$p)

write.csv(desc_df.rcx.r,'./exports/corr_matrix.csv')
write.csv(desc_df.rcx.p,'./exports/corr_matrix_pvals.csv')

################################################################################
# PRODUCER-AUDIENCE CONFUSION MODELS (DV: SIMILARITY OF PRODUCER/AUDIENCE TAGS)
# TABLE 2

model0 = lm(jacc ~ price_x +
              rating +
              franchise +
              owners_x +
              genre_count, data=data)

model1 = lm(jacc ~ price_x +
              rating +
              franchise +
              owners_x +
              genre_count +
              TYPICALITY, data=data)

################################################################################
# AUDIENCE-AUDIENCE CONSENSUS MODELS (DV: ENTROPY OF AUDIENCE TAGS)

model2 = lm(entropy ~ price_x +
              rating +
              franchise +
              owners_x +
              genre_count, data=data)

model3 = lm(entropy ~ price_x +
              rating +
              franchise +
              owners_x +
              genre_count +
              TYPICALITY, data=data)

# OUTPUT TO HTML

stargazer(model0, model1, model2, model3,
          type="html",
          out = "./exports/regression_models.htm",
          covariate.labels=c("Price", "Rating", "Franchise game","Number of owners",
                             "Number of genres", "Producer tags typicality"),
          header = FALSE, 
          df = FALSE,
          digits=2,
          single.row = FALSE,
          align=TRUE,
          digits=2
)

################################################################################
# PLOT BIVARIATE RELATIONSHIPS BETWEEN DVS AND IV

# FIGURE 1


require(ggthemes)
require(ggplot2)

f1 = ggplot(data, aes(x = TYPICALITY, y = jacc)) +
  geom_point() +
  xlab("Typicality of producer tags") + 
  ylab("Producer-audience tags similarity") +
  coord_cartesian(xlim = c(0,0.1), ylim = c(0,0.4), expand = FALSE) +
  geom_smooth(method='lm') +
  theme_tufte() +
  geom_rangeframe() +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave("./exports/fig1.png", plot = f1)


f2 = ggplot(data, aes(x = TYPICALITY, y = entropy)) +
  geom_point() +
  xlab("Typicality of producer tags") + 
  ylab("Entropy of audience tags") +
  coord_cartesian(xlim = c(0,0.15), ylim = c(0,4), expand = FALSE) +
  geom_smooth(method='lm') +
  theme_tufte() +
  geom_rangeframe() +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

ggsave("./exports/fig2.png", plot = f2)


f3

require(ggthemes)
library(ggplot2)
f4 <- ggplot(data, aes(x = TYPICALITY , y = total_revs)) + 
  geom_point()  + 
  xlab("Typicality of producer tags") + 
  ylab("Total number of reviews") +
  coord_cartesian(xlim = c(0,0.09), ylim = c(0,5000), expand = FALSE) +
  # scale_x_continuous(name = "Typicality of producer tags", limits = c(0, 0.1)) +
  # scale_y_continuous(name = "Total number of user reviews", limits = c(0, 5000)) +
  geom_smooth(method='lm') +
  geom_rangeframe() +
  theme_tufte() +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))


f4

ggsave("./exports/fig4.png", plot = f4)







cor.test(data$TYPICALITY,data$total_revs)


ggsave("./exports/fig3.png", plot = f3)







data$franchise

################################################################################
# DOES SPANNING ATTRACT LARGER AUDIENCE?

temp_df = data[data$total_tags_count_t2<1000 & data$TYPICALITY<0.2,]

plot(temp_df$TYPICALITY, temp_df$total_tags_count_t2)
abline(lm(temp_df$total_tags_count_t2 ~ temp_df$TYPICALITY))

summary(lm(data$total_tags_count_t2 ~ data$TYPICALITY + data$owners_x))          # YES ?

################################################################################
#WHAT HAPPENS WHEN YOU CONTROL FOR TOTAL # OF T2 TAGS?                          #log(total_t2_tags) is even better
       

model0 = lm(jacc ~ price_x +
              rating +
              franchise +
              total_tags_count_t2 +
              owners_x +
              genre_count, data=data)

model1 = lm(jacc ~ price_x +
              rating +
              franchise +
              total_tags_count_t2 +
              owners_x +
              genre_count +
              TYPICALITY, data=data)

################################################################################
# AUDIENCE-AUDIENCE CONSENSUS MODELS (DV: ENTROPY OF AUDIENCE TAGS)

model2 = lm(entropy ~ price_x +
              rating +
              franchise +
              total_tags_count_t2 +
              owners_x +
              genre_count, data=data)

model3 = lm(  owners_x + ~ price_x +
              rating +
              franchise +
              total_tags_count_t2 +
              genre_count +
              TYPICALITY, data=data)

# OUTPUT TO HTML

stargazer(model0, model1, model2, model3,
          type="text",
          # out = "./exports/regression_models.htm",
          covariate.labels=c("Price", "Rating", "Franchise game", 
                             "Number of t2 tags", "Number of owners",
                             "Number of genres", "Producer tags typicality"),    # MODEL GETS BETTER
          header = FALSE, 
          df = FALSE,
          digits=2,
          single.row = FALSE,
          align=TRUE
)

        