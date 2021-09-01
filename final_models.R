# IMPORT AND AND MERGE DATA
setwd("C:/Users/gaoan/Desktop/steamspyanalyzer")
rm(list=ls())

df = read.csv("step12_result.csv")
df$positivity = df$pos_rev_num / (df$pos_rev_num + df$neg_rev_num)
df$total_revs = df$pos_rev_num + df$neg_rev_num

# colnames(df)[542:966]

df$num_of_tags_t2 = rowSums(df[542:966]!=0)

df$num_of_tags_t1 = rowSums(df[115:539]!=0)

ent_change_vec = vector()

jacc_vec = vector()

ent_vec = vector()

# CALCULATE AUDIENCE ENTROPY (using vector length = 425)

require("entropy")

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

# # CALCULATE CHANGE IN ENTROPY
# 
# for (i in 1: nrow(df)){
#   
#   g1 = df[,542:966]
#   g1 = g1[i,0:425]
#   
#   # summary(df$t2_entropy) # FROM 0 ~ 3.9
#   
#   # low_ent_game_t2_tags = df[df$t2_entropy<2 & df$num_of_tags>4, 542:966]
#   # rand_game_num = round(runif(1, 1, nrow(low_ent_game_t2_tags)),0)
#   # g1 = low_ent_game_t2_tags[rand_game_num,0:425]
#   
#   # high_ent_game_t2_tags = df[df$t2_entropy>3 & df$num_of_tags>5, 542:966]
#   # rand_game_num = round(runif(1, 1, nrow(high_ent_game_t2_tags)),0)
#   # g1 = high_ent_game_t2_tags[rand_game_num,0:425]
#   
#   
#   df_t = as.data.frame(t(g1))
#   colnames(df_t) = "count"
#   df_t$count = as.integer(df_t$count)
#   df_t$tag_names = rownames(df_t)
#   t2_pos_tag_df = df_t[df_t$count > 0,]
#   
#   require("entropy")
#   freqs = freqs.empirical(t2_pos_tag_df$count)
#   ent_t2 = entropy.empirical(freqs, unit="log2")
#   
#   g2 = df[,115:539]
#   g2 = g2[i,0:425]
#   
#   # summary(df$t2_entropy) # FROM 0 ~ 3.9
#   
#   # low_ent_game_t2_tags = df[df$t2_entropy<2 & df$num_of_tags>4, 542:966]
#   # rand_game_num = round(runif(1, 1, nrow(low_ent_game_t2_tags)),0)
#   # g1 = low_ent_game_t2_tags[rand_game_num,0:425]
#   
#   # high_ent_game_t2_tags = df[df$t2_entropy>3 & df$num_of_tags>5, 542:966]
#   # rand_game_num = round(runif(1, 1, nrow(high_ent_game_t2_tags)),0)
#   # g1 = high_ent_game_t2_tags[rand_game_num,0:425]
#   
#   df_t = as.data.frame(t(g2))
#   colnames(df_t) = "count"
#   df_t$count = as.integer(df_t$count)
#   df_t$tag_names = rownames(df_t)
#   t2_pos_tag_df = df_t[df_t$count > 0,]
#   
#   require("entropy")
#   freqs = freqs.empirical(t2_pos_tag_df$count)
#   ent_t1 = entropy.empirical(freqs, unit="log2")
#   
#   entropy_change = ent_t2 - ent_t1
#   
#   ent_change_vec = c(ent_change_vec, entropy_change)
#   
# }
# 
# df$ent_change = ent_change_vec


################################################################################
# MERGE IN OTHER MEASURES AND CREATE SUBSET

df = df[,c('app_id','price_x', 'positivity', 'owners_x', 'genre_1',
           'replaced_tags_count','t2_entropy', 'genre_count',
           'num_of_tags_t1','num_of_tags_t2','jacc', 'entropy', 'total_revs')]

df = df[df$genre_count!=0,]
df = df[!duplicated(df),]


cos_sim_df = read.csv("cos_sim.csv")
colnames(cos_sim_df)[1] = "app_id"
cos_sim_df = cos_sim_df[!duplicated(cos_sim_df),]

data = merge(df,cos_sim_df,by="app_id", all=F)

typ = read.csv("typicality.csv")
colnames(typ)[1] = "app_id"
typ = typ[!duplicated(typ),]

data = merge(data,typ,by="app_id", all=F)

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
data$genre_1 = as.factor(data$genre_1)

################################################################################
# DESCRIPTIVES & BIVARIATE CORRELATIONS

library(dplyr)
library(magrittr)
new_df = data %>% select(price_x, rating, franchise, owners_x, num_of_tags_t2,
                         novel_tags_count,
                         TYPICALITY, jac_sim_equal_length, t2_entropy, positivity, owners_x)

# GET DESCRIPTIVE STATISTICS
library(psych)
descriptives = describe(new_df)

# write.table(descriptives,file="describe.csv",sep=",")

# MAKE CORRELATION TABLE

# dcor<-round(cor(new_df),2)
# upper<-dcor
# upper[upper.tri(dcor)]<-""
# upper<-as.data.frame(upper)
# library(xtable)
# print(xtable(upper), type="html")
# print(xtable(upper), file="table.csv")


# PLOT BIVARIATE CORRELATIONS

plot(data$TYPICALITY,data$jacc)
abline(lm(data$jacc~data$TYPICALITY))

plot(data$jacc[data$jacc != 1]~data$genre_count[data$jacc != 1])
abline(lm(data$jacc[data$jacc != 1]~data$genre_count[data$jacc != 1]))

plot(data$TYPICALITY,data$entropy)
abline(lm(data$t2_entropy~data$TYPICALITY))

################################################################################
#MODELS

# # DV: JAC SIM BTW t0 and t1 (equal length) tag vectors (IV: GENRE COUNT)
# 
# model0 = lm(jac_sim_equal_length ~ price_x +
#               positivity +
#               owners_x +
#               genre_1, data=data)
# 
# model1 = lm(jac_sim_equal_length ~ price_x +
#               positivity +
#               owners_x +
#               genre_1 +
#               genre_count, data=data)
# 
# require(stargazer)
# stargazer(model0,model1,type="text")


# DV: JAC SIM BTW t0 and t1 (equal length) tag vectors (IV: TYPICALITY)

# model0 = lm(jac_sim_equal_length ~
#               price_x +
#               rating +
#               franchise +
#               owners_x +
#               num_of_tags_t1 +
#               num_of_tags_t2, data=data)
# 
# model1 = lm(jac_sim_equal_length ~
#               price_x +
#               rating +
#               franchise +
#               owners_x +
#               num_of_tags_t1 +
#               num_of_tags_t2 +
#               TYPICALITY, data=data)
# 
# stargazer(model0,model1,
#           type="html",
#           out = "jac_sim_models.htm",
#           covariate.labels=c("Price", "Rating", "Franchise game","Number of owners", 
#                              "Total producer tags", "Total audience tags", "Producer tags typicality"),
#           header = FALSE, 
#           df = FALSE,
#           digits=2,
#           single.row = FALSE                  
# )

# require(stargazer)
# stargazer(model0,model1,type="text",
#           covariate.labels=c("Price", "Rating", "Franchise game","Number of owners",
#                              "Total producer tags", "Total audience tags", "Producer tags typicality"),
#           header = FALSE,
#           df = FALSE,
#           digits=2,
#           single.row = FALSE
#           )



# model2 = lm(jac_sim_equal_length ~
#               price_x +
#               rating +
#               franchise +
#               owners_x +
#               num_of_tags_t2 +
#               novel_tags_count +
#               poly(TYPICALITY, 2), data=data)
# 
# require(stargazer)
# stargazer(model0,model1,model2,type="text")



# # DV: JAC SIM BTW t0 and t1 (equal length) tag vectors (IV: COS SIM)
# 
# model0 = lm(jac_sim_equal_length ~ price_x +
#               positivity +
#               owners_x, data=data)
# 
# model1 = lm(jac_sim_equal_length ~ price_x +
#               positivity +
#               owners_x +
#               cos_sim, data=data)
# 
# model2 = lm(jac_sim_equal_length ~ price_x +
#               positivity +
#               owners_x +
#               poly(cos_sim, 2), data=data)
# 
# 
# 
# require(stargazer)
# stargazer(model0,model1,model2,type="text")




# # DV: HOW MANY TAGS GET REPLACED (IV: GENRE COUNT)
# 
# model0 = lm(replaced_tags_count ~ price_x +
#                    positivity +
#                    owners_x +
#                    genre_1, data=data)
# 
# model1 = lm(replaced_tags_count ~ price_x +
#                    positivity +
#                    owners_x +
#                    genre_1 +
#                    genre_count, data=data)
# 
# require(stargazer)
# stargazer(model0,model1,type="text")
# 
# 
# # DV: HOW MANY TAGS GET REPLACED (IV: TYPICALITY)
# 
# 
# model0 = lm(replaced_tags_count ~ price_x +
#               positivity +
#               owners_x, data=data)
# 
# model1 = lm(replaced_tags_count ~ price_x +
#               positivity +
#               owners_x +
#               TYPICALITY, data=data)
# 
# require(stargazer)
# stargazer(model0,model1,type="text")
# 
# # DV: HOW MANY TAGS GET REPLACED (IV: COS_SIM)
# 
# 
# model0 = lm(replaced_tags_count ~ price_x +
#               positivity +
#               owners_x, data=data)
# 
# model1 = lm(replaced_tags_count ~ price_x +
#               positivity +
#               owners_x +
#               TYPICALITY, data=data)
# 
# require(stargazer)
# stargazer(model0,model1,type="text")


# # DV: ENTROPY OF AUDIENCE TAGS (IV: GENRE COUNT)
# 
# model0 = lm(t2_entropy ~ price_x +
#               positivity +
#               owners_x +
#               genre_1, data=data)
# 
# model1 = lm(t2_entropy ~ price_x +
#               positivity +
#               owners_x +
#               genre_1 +
#               genre_count, data=data)
# 
# require(stargazer)
# stargazer(model0,model1,type="text")

# DV: HOW MANY TAGS GET REPLACED (IV: TYPICALITY)


# model0 = lm(t2_entropy ~ price_x +
#               rating +
#               franchise +
#               owners_x +
#               num_of_tags_t1 +
#               num_of_tags_t2, data=data)
# 
# model1 = lm(t2_entropy ~ price_x +
#               rating +
#               franchise +
#               owners_x +
#               num_of_tags_t1 +
#               num_of_tags_t2 +
#               TYPICALITY, data=data)
# 
# stargazer(model0,model1,
#           type="html",
#           out = "entropy_models.htm",
#           covariate.labels=c("Price", "Rating", "Franchise game","Number of owners", 
#                              "Total producer tags", "Total audience tags", "Producer tags typicality"),
#           header = FALSE, 
#           df = FALSE,
#           digits=2,
#           single.row = FALSE                  
# )
# 
# stargazer(model0,model1,
#           type="text",
#           covariate.labels=c("Price", "Rating", "Franchise game","Number of owners", 
#                              "Total producer tags", "Total audience tags", "Producer tags typicality"),
#           header = FALSE, 
#           df = FALSE,
#           digits=2,
#           single.row = FALSE                  
# )
# 
# model0 = lm(t2_entropy ~ num_of_tags_t1, data=data)
# 
# plot(data$num_of_tags_t1, data$t2_entropy)
# 
# summary(model0)


# model2 = lm(t2_entropy ~ price_x +
#               rating +
#               franchise +
#               owners_x +
#               novel_tags_count +
#               num_of_tags_t2 +
#               poly(TYPICALITY, 2), data=data)
# 
# require(stargazer)
# stargazer(model0,model1,model2,type="text")

# # DV: HOW MANY TAGS GET REPLACED (IV: COSINE SIMILARITY)
# 
# 
# model0 = lm(t2_entropy ~ price_x +
#               positivity +
#               owners_x, data=data)
# 
# model1 = lm(t2_entropy ~ price_x +
#               positivity +
#               owners_x +
#               cos_sim, data=data)
# 
# model2 = lm(t2_entropy ~ price_x +
#               positivity +
#               owners_x +
#               poly(cos_sim,2), data=data)
# 
# require(stargazer)
# stargazer(model0,model1,model2,type="text")

###########################################################
# TRY BOTH GENRES AND TYPICALITY IN SAME MODEL (DV: ENTROPY)

model0 = lm(entropy ~ price_x +
              rating +
              franchise +
              owners_x +
              # genre_count +
              num_of_tags_t1, data=data)

model1 = lm(entropy ~ price_x +
              rating +
              franchise +
              owners_x +
              # genre_count +
              num_of_tags_t1 +
              TYPICALITY, data=data)

stargazer(model0,model1,
          type="text",
          # covariate.labels=c("Price", "Rating", "Franchise game","Number of owners", 
          #                    "Number NUmber of genres"),
          header = FALSE, 
          df = FALSE,
          digits=2,
          single.row = FALSE                  
)

# TRY BOTH GENRES AND TYPICALITY IN SAME MODEL (DV: JACCARD)

model0 = lm(jacc ~ price_x +
              rating +
              franchise +
              owners_x +
              # genre_count +
              num_of_tags_t1, data=data)

model1 = lm(jacc ~ price_x +
              rating +
              franchise +
              owners_x +
              # genre_count +              
              num_of_tags_t1 +
              TYPICALITY, data=data)

stargazer(model0,model1,
          type="text",
          # covariate.labels=c("Price", "Rating", "Franchise game","Number of owners", 
          #                    "Number NUmber of genres"),
          header = FALSE, 
          df = FALSE,
          digits=2,
          single.row = FALSE                  
)

plot(data$genre_count,data$TYPICALITY)

mean(data$TYPICALITY[data$genre_count == 2])
mean(data$TYPICALITY[data$genre_count == 4])
mean(data$TYPICALITY[data$genre_count == 6])
mean(data$TYPICALITY[data$genre_count == 8])

cor(data$TYPICALITY, data$novel_tags_count)

cor(data$genre_count, data$novel_tags_count)

plot(data$cos_sim,data$t2_entropy)
cor.test(data$cos_sim,data$t2_entropy)

plot(data$TYPICALITY[data$jacc!=1],data$jacc[data$jacc!=1])
abline(lm(data$jacc[data$jacc!=1] ~ data$TYPICALITY[data$jacc!=1]))

cor.test(data$cos_sim,data$jac_sim)


plot(data$jacc[data$jacc!=1],data$rating[data$jacc!=1])
abline(lm(data$rating[data$jacc!=1] ~ data$jacc[data$jacc!=1]))

plot(data$jacc[data$jacc!=1],data$rating[data$jacc!=1])
abline(lm(data$rating[data$jacc!=1] ~ data$jacc[data$jacc!=1]))

plot(data$jacc[data$jacc > 0.047 & data$jacc < 0.4], data$rating[data$jacc > 0.047 & data$jacc < 0.4])

abline(lm(data$rating[data$jacc > 0.047 & data$jacc < 0.4] ~ poly(data$jacc[data$jacc > 0.047 & data$jacc < 0.4],2)))

plot(data$t2_entropy[data$jacc!=1],data$rating[data$jacc!=1])
abline(lm(data$rating[data$jacc!=1] ~ data$t2_entropy[data$jacc!=1]))

mean(data$rating[data$jacc == 2])
mean(data$rating[data$jacc == 4])
mean(data$rating[data$jacc == 6])
mean(data$rating[data$jacc == 8])



#################################### TOBIT

# NOT NECESSARY


require(VGAM)

options(scipen=9999)

tobit_model0 = vglm(jacc ~ price_x +
                      rating +
                      franchise +
                      owners_x +
                      genre_count, family=tobit(Lower=0), data=data)

summary(tobit_model0)

tobit_model1 = vglm(jacc ~ price_x +
              rating +
              franchise +
              owners_x +
              genre_count +
              TYPICALITY, family=tobit(Lower=0), data=data)

summary(tobit_model1)







########################

# PLOT 

library(ggplot2)

ggplot(data, aes(x = TYPICALITY, y = jac_sim_equal_length)) +
  theme_bw() +
  geom_smooth(method=lm) +
  geom_point()

# # WITH 2ND ORDER AND CONF. INTS.
# library(ggplot2)
# fit <- lm(jac_sim_equal_length ~ TYPICALITY + TYPICALITY^2, data = data)
# prd <- data.frame(TYPICALITY = seq(from = range(data$TYPICALITY)[1], to = range(data$TYPICALITY)[2], length.out = 100))
# err <- predict(fit, newdata = prd, se.fit = TRUE)
# 
# prd$lci <- err$fit - 1.96 * err$se.fit
# prd$fit <- err$fit
# prd$uci <- err$fit + 1.96 * err$se.fit
# 
# ggplot(prd, aes(x = TYPICALITY, y = fit)) +
#   theme_bw() +
#   geom_line() +
#   geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
#   geom_point(data = data, aes(x = TYPICALITY, y = jac_sim_equal_length))

ggplot(data, aes(x = TYPICALITY, y = t2_entropy)) +
  theme_bw() +
  geom_smooth(method=lm) +
  geom_point()


# # WITH 2ND ORDER AND CONF. INTS.
# library(ggplot2)
# fit <- lm(t2_entropy ~ TYPICALITY + TYPICALITY^2, data = data)
# prd <- data.frame(TYPICALITY = seq(from = range(data$TYPICALITY)[1], to = range(data$TYPICALITY)[2], length.out = 100))
# err <- predict(fit, newdata = prd, se.fit = TRUE)
# 
# prd$lci <- err$fit - 1.96 * err$se.fit
# prd$fit <- err$fit
# prd$uci <- err$fit + 1.96 * err$se.fit
# 
# ggplot(prd, aes(x = TYPICALITY, y = fit)) +
#   theme_bw() +
#   geom_line() +
#   geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
#   geom_point(data = data, aes(x = TYPICALITY, y = t2_entropy))


ggplot(data, aes(x = t2_entropy, y = rating)) +
  theme_bw() +
  geom_smooth(method=lm) +
  geom_point()

ggplot(data, aes(x = jac_sim_equal_length, y = rating)) +
  theme_bw() +
  geom_smooth(method=lm) +
  geom_point()

model1 = lm(rating ~ price_x +
              jac_sim_equal_length +
              franchise +
              owners_x +
              novel_tags_count, data=data)
summary(model1)



#MEAN VALUES

mean(data$price_x)
mean(data$rating)
mean(data$owners_x)
mean(data$novel_tags_count)

?entropy




################################################################################



################################################################################
#DV: RATING              

model0 = lm(rating ~ price_x +
              owners_x +
              franchise +
              genre_count, data=data)

model1 = lm(rating ~ price_x +
              owners_x +
              franchise +
              TYPICALITY, data=data)

model2 = lm(rating ~ price_x +
              owners_x +
              franchise +
              jacc, data=data)

model3 = lm(rating ~ price_x +
              owners_x +
              franchise +
              entropy, data=data)

model4 = lm(rating ~ price_x +
              owners_x +
              franchise +
              jacc +
              entropy, data=data)

model5 = lm(rating ~ price_x +
              owners_x +
              franchise +
              genre_count +
              TYPICALITY+
              jacc +
              entropy, data=data)


# OUTPUT TO HTML

stargazer(model0, model1, model2, model3, model4, model5,
          type="text",
          # # out = "./exports/regression_models.htm",
          # covariate.labels=c("Price", "Rating", "Franchise game", 
          #                    "Number of genres", "Producer tags typicality"),   
          header = FALSE, 
          df = FALSE,
          digits=2,
          single.row = FALSE,
          align=TRUE
)


#DV: NUMBER OF OWNERS    

model0 = lm(owners_x ~ price_x +
              rating +
              franchise +
              genre_count, data=data)

model1 = lm(owners_x ~ price_x +
              rating +
              franchise +
              TYPICALITY, data=data)

model2 = lm(owners_x ~ price_x +
              rating +
              franchise +
              jacc, data=data)

model3 = lm(owners_x ~ price_x +
              rating +
              franchise +
              entropy, data=data)


model4 = lm(owners_x ~ price_x +
              rating +
              franchise +
              jacc +
              entropy, data=data)

model5 = lm(owners_x ~ price_x +
              rating +
              franchise +
              genre_count +
              TYPICALITY+
              jacc +
              entropy, data=data)


# OUTPUT TO HTML

stargazer(model0, model1, model2, model3, model4, model5,
          type="text",
          # # out = "./exports/regression_models.htm",
          # covariate.labels=c("Price", "Rating", "Franchise game", 
          #                    "Number of genres", "Producer tags typicality"),   
          header = FALSE, 
          df = FALSE,
          digits=2,
          single.row = FALSE,
          align=TRUE
)


################################################################################
# MEDIATION MODELS

model1 = lm(owners_x ~ price_x +
              rating +
              franchise, data=data)

model2 = lm(owners_x ~ price_x +
              rating +
              franchise +
              #genre_count +
              #TYPICALITY +
              jacc, data=data)

model3 = lm(owners_x ~ price_x +
              rating +
              franchise +
              #genre_count +
              #TYPICALITY +
              entropy, data=data)

model4 = lm(owners_x ~ price_x +
              rating +
              franchise +
              #genre_count +
              #TYPICALITY +
              jacc +
              entropy, data=data)



# OUTPUT TO HTML

stargazer(model1, model2, model3, model4,
          type="text",
          # # out = "./exports/regression_models.htm",
          # covariate.labels=c("Price", "Rating", "Franchise game", 
          #                    "Number of genres", "Producer tags typicality"),   
          header = FALSE, 
          df = FALSE,
          digits=2,
          single.row = FALSE,
          align=TRUE
)


################################################################################


library(stargazer)

# DV: total tags applied




model1 = lm(rating ~ price_x +
              #num_of_tags_t2 +
              #owners_x +
              #genre_1 +
              franchise, data=data)

# model2 = lm(total_revs ~ price_x +
#               rating +
#               franchise +
#               genre_1 +
#               TYPICALITY +
#               jacc, data=data)
# 
# 
# model3 = lm(total_revs ~ price_x +
#               rating +
#               franchise +
#               genre_1 +
#               TYPICALITY +
#               entropy, data=data)

model4 = lm(rating ~ price_x +
              #num_of_tags_t2 +
              #owners_x +
              franchise +
              genre_1 +
              #genre_count+
              #TYPICALITY +
              poly(jacc,2), data=data)



model5 = lm(rating ~ price_x +
              #num_of_tags_t2 +
              #owners_x +
              franchise +
              genre_1 +
              #genre_count+
              #TYPICALITY +
              entropy, data=data)

model6 = lm(rating ~ price_x +
              #num_of_tags_t2 +
              #owners_x +
              genre_1 +
              franchise +
              #genre_1 +
              #genre_count+
              #TYPICALITY +
              poly(jacc,2) +
              entropy, data=data)


stargazer(model1, model4, model5, model6,
          type="text",
          # # out = "./exports/regression_models.htm",
          # covariate.labels=c("Price", "Rating", "Franchise game", 
          #                    "Number of genres", "Producer tags typicality"),   
          header = FALSE, 
          df = FALSE,
          digits=2,
          single.row = FALSE,
          align=TRUE
)

plot()


fit2 <- lm(data$rating[data$jacc!=1] ~ data$jacc[data$jacc!=1] + I(data$jacc[data$jacc!=1]^2))

pol2 <- function(x) fit2$coefficient[3]*x^2 + fit2$coefficient[2]*x + fit2$coefficient[1]
plot(data$jacc[data$jacc!=1], data$rating[data$jacc!=1], type="p", lwd=3)
curve(pol2, col="red", lwd=2, add=T)




# DV: TOTAL REVIEWS


model1 = lm(total_revs ~ price_x +
              positivity +
              franchise +
              genre_1 +
              cos_sim, data=data)

# model2 = lm(total_revs ~ price_x +
#               rating +
#               franchise +
#               genre_1 +
#               TYPICALITY +
#               jacc, data=data)
# 
# 
# model3 = lm(total_revs ~ price_x +
#               rating +
#               franchise +
#               genre_1 +
#               TYPICALITY +
#               entropy, data=data)

model4 = lm(total_revs ~ price_x +
              positivity +
              franchise +
              genre_1 +
              cos_sim +
              jacc, data=data)

model5 = lm(total_revs ~ price_x +
              positivity +
              franchise +
              genre_1 +
              cos_sim +
              entropy, data=data)


stargazer(model1, model4,model5,
          type="text",
          # # out = "./exports/regression_models.htm",
          # covariate.labels=c("Price", "Rating", "Franchise game", 
          #                    "Number of genres", "Producer tags typicality"),   
          header = FALSE, 
          df = FALSE,
          digits=2,
          single.row = FALSE,
          align=TRUE
)



summary(df$positivity)

hist(df$entropy)





plot(data$TYPICALITY, data$cos_sim)


plot(data$cos_sim,data$total_revs)

plot(data$cos_sim,data$rating)

plot(data$cos_sim,data$positivity)


plot(data$jacc**2,data$rating)

plot(data$entropy**2,data$rating)


summary(lm(data$rating~data$jacc + data$entropy))


mean(data$entropy) + sd(data$entropy) #HIGH ENTROPY


mean(data$entropy) - sd(data$entropy) #LOW ENTROPY



mean(data$jacc) + sd(data$jacc) #HIGH SIMILARITY


mean(data$jacc) - sd(data$jacc) #LOW SIMILARITY


summary(data$rating)

mean(data$entropy[data$rating>=80])

mean(data$entropy[data$rating<=60])

summary(data$jacc)

mean(data$jacc[data$rating>=80])

mean(data$jacc[data$rating<=60])


plot(data$jacc, data$rating)


install.packages("rgl") # Install
library("rgl") # load
install.packages("htmltools") 

install.packages(("rlang"))

list.of.packages <- c("rgl","ggplot2","knitr","rglwidget")
#
#You should be able to simply reuse the following lines of code as is
#
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#
if(length(new.packages)) install.packages(new.packages)
#
# By now we have installed the requisite packages. Time to load them .
#
lapply(list.of.packages,function(x){library(x,character.only=TRUE)})


plot3d(data$entropy, data$jacc, data$rating, type="s", size=1, lit=TRUE)

data$total_tags_count_t2 = 








# # 3D scatter plot
# s3d <- scatterplot3d(data$entropy, data$jacc, data$rating, color = "blue",
#                      angle=55, pch = 16)
# # Add regression plane
# my.lm <- lm(data$rating ~ data$entropy + data$jacc)
# s3d$plane3d(my.lm)


summary(data$num_of_tags_t2)

hist(data$num_of_tags_t2, breaks = 100)
hist(log(data$total_tags_count_t2), breaks = 100)

hist(data$price, breaks = 10)
hist(log(data$price), breaks = 10)

hist(data$rating, breaks = 100)
hist(log(data$rating), breaks = 100)

hist(data$owners_x, breaks = 100)
hist(log(data$owners_x), breaks = 100)

hist(data$entropy, breaks = 100)
hist(sqrt(data$entropy), breaks = 100)

hist(data$jacc, breaks = 100)
hist(log(data$jacc), breaks = 100)

par(mfrow = c(1, 1))
plot(model0)


cor.test()

library(stargazer)

#DV: REPLACED TAGS COUNT            

model0 = lm(replaced_tags_count ~ price_x +
              rating +
              owners_x +
              franchise, data=data)

model1 = lm(replaced_tags_count ~ price_x +
              rating +
              owners_x +
              franchise +
              genre_count, data=data)

model2 = lm(replaced_tags_count ~ price_x +
              rating +
              owners_x +
              franchise +
              genre_count +
              TYPICALITY, data=data)



# OUTPUT TO HTML

stargazer(model0, model1, model2,
          type="text",
          # # out = "./exports/regression_models.htm",
          # covariate.labels=c("Price", "Rating", "Franchise game", 
          #                    "Number of genres", "Producer tags typicality"),   
          header = FALSE, 
          df = FALSE,
          digits=2,
          single.row = FALSE,
          align=TRUE
)


summary(lm( rating ~ price_x +
     rating +
     owners_x +
     franchise +
     TYPICALITY, data=data))


summary(lm(positivity ~ price_x +
             total_revs +
             franchise +
             owners_x +
             genre_count +
             jacc_distance, data=data))



data$jacc_distance = -1 * (log(data$jacc) / 0.5)



summary(lm(jacc_distance ~ price_x +
             rating +
             franchise +
             total_revs +
             TYPICALITY, data=data))



plot(data$TYPICALITY, data$total_revs, xlim=0.09, ylim = 5000)












plot(data$num_of_tags_t2, data$entropy)

summary(data$entropy)




#### MAKE A PLOT OF DISTRIBUTIONS OF TAGS



require(ggthemes)

df2 = read.csv("step12_result.csv")

# FIND GAME WITH LOW ENTROPY LOW TAG COUNT
#data[data$entropy < 2 & data$num_of_tags_t2 < 8, c("app_id", "entropy", "num_of_tags_t2")]


tags1 = df2[df2$app_id==473610  ,542:966]
rownames(tags1) = "count"
tags1 = tags1[, colSums(tags1 != 0) > 0]
tags1 = t(tags1)
tags1 = as.data.frame(tags1)
# barplot(tags$count, names.arg=rownames(tags))

p1 = ggplot(tags1, aes(x=rownames(tags1), y=count)) + 
  geom_bar(stat='identity') +
  ggtitle("Low entropy, low tag count") +
  scale_x_discrete("",expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0,50), expand = c(0, 0)) +
  theme_tufte() +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))


plot1 = p1

# FIND GAME WITH HIGH ENTROPY LOW TAG COUNT
#data[data$entropy > 2.7 & data$num_of_tags_t2 < 8, c("app_id", "entropy", "num_of_tags_t2")]

tags2 = df2[df2$app_id==295950 ,542:966]
tags2 = tags2[, colSums(tags2 != 0) > 0]
tags2 = t(tags2)
colnames(tags2) = "count"
tags2 = as.data.frame(tags2)
# barplot(tags$count, names.arg=rownames(tags))

p2 = ggplot(tags2, aes(x=rownames(tags2), y=count)) + 
  geom_bar(stat='identity') +
  ggtitle("High entropy, low tag count") +
  scale_x_discrete("",expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0,50), expand = c(0, 0)) +
  theme_tufte() +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))


# FIND GAME WITH LOW ENTROPY HIGH TAG COUNT
#data[data$entropy < 2.5 & data$num_of_tags_t2 > 6, c("app_id", "entropy", "num_of_tags_t2")]

tags3 = df2[df2$app_id==545270,542:966]
tags3 = tags3[, colSums(tags3 != 0) > 0]
tags3 = t(tags3)
colnames(tags3) = "count"
tags3 = as.data.frame(tags3)
# barplot(tags$count, names.arg=rownames(tags))

p3 = ggplot(tags3, aes(x=rownames(tags3), y=count)) + 
  geom_bar(stat='identity') +
  ggtitle("Low entropy, high tag count") +
  scale_x_discrete("",expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0,50), expand = c(0, 0)) +
  theme_tufte() +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))

plot3 = p3


# FIND GAME WITH HIGH ENTROPY HIGH TAG COUNT
#data[data$entropy > 3 & data$num_of_tags_t2 > 13, c("app_id", "entropy", "num_of_tags_t2")]

tags4 = df2[df2$app_id==691630,542:966]
tags4 = tags4[, colSums(tags4 != 0) > 0]
tags4 = t(tags4)
colnames(tags4) = "count"
tags4 = as.data.frame(tags4)
# barplot(tags$count, names.arg=rownames(tags))


p4 = ggplot(tags4, aes(x=rownames(tags4), y=count)) + 
  geom_bar(stat='identity') +
  ggtitle("High entropy, high tag count") +
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0,50), expand = c(0, 0)) +
  theme_tufte() +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5))


g <- arrangeGrob(p3, p4, p1, p2, nrow=2) #generates g
ggsave(file="./exports/fig3.png", g)  


require(gridExtra)

grid.arrange(p3, p4, p1, p2, ncol=2)
  

cor.test(data$TYPICALITY, data$total_revs)



#### WHAT HAPPENS IF I REVERSE THE MODEL AND USE TYPICALITY AS DV




model1 = lm(TYPICALITY ~ price_x +
              rating +
              franchise +
              owners_x, data=data)

model2 = lm(TYPICALITY ~ price_x +
              rating +
              franchise +
              owners_x +
              jacc, data=data)

model3 = lm(TYPICALITY ~ price_x +
              rating +
              franchise +
              owners_x +
              entropy, data=data)

model4 = lm(TYPICALITY ~ price_x +
              rating +
              franchise +
              owners_x +
              jacc +
              entropy, data=data)

# OUTPUT TO HTML

stargazer(model1, model2, model3, model4,
          type="text",
          header = FALSE, 
          df = FALSE,
          digits=2,
          single.row = FALSE,
          align=TRUE
)

#### WHAT HAPPENS IF entropy=IV  TYPICALITY= moderator, similarity=DV

model1 = lm(jacc ~ price_x +
              rating +
              franchise +
              genre_count +
              owners_x +
              num_of_tags_t1 +
              num_of_tags_t2, data=data)

model2 = lm(jacc ~ price_x +
              rating +
              franchise +
              genre_count +
              owners_x +
              num_of_tags_t1 +
              num_of_tags_t2 +
              entropy, data=data)

model3 = lm(jacc ~ price_x +
              rating +
              franchise +
              genre_count +
              owners_x +
              num_of_tags_t1 +
              num_of_tags_t2 +
              entropy +
              TYPICALITY, data=data)

model4 = lm(jacc ~ price_x +
              rating +
              franchise +
              genre_count +
              owners_x +
              num_of_tags_t1 +
              num_of_tags_t2 +
              entropy +
              TYPICALITY +
              entropy*TYPICALITY, data=data)

# OUTPUT TO HTML

stargazer(model1, model2, model3, model4,
          type="text",
          header = FALSE, 
          df = FALSE,
          digits=2,
          single.row = FALSE,
          align=TRUE
)

# INTERACTION PLOT

library(sjPlot)
library(sjmisc)
library(ggplot2)

# fit model with interaction
fit <- lm(jacc ~ price_x +
            rating +
            franchise +
            genre_count +
            owners_x +
            num_of_tags_t1 +
            num_of_tags_t2 +
            entropy +
            TYPICALITY +
            entropy*TYPICALITY, data=data)

plot_model(fit, type = "pred", terms = c("entropy", "TYPICALITY [0.01, 0.14]"))


summary(data$TYPICALITY)









  