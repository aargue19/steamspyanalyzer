setwd("C:/Users/gaoan/Desktop/steamspyanalyzer")
rm(list=ls())

df = read.csv("step12_result.csv")
df$positivity = df$pos_rev_num / (df$pos_rev_num + df$neg_rev_num)
# colnames(df[,1:120])

df = df[,c('app_id','price_x', 'positivity', 'owners_x', 'followers_scrap', 'genre_1',
           'jac_sim', 'novel_tags_count','t2_entropy', 'genre_count')]

df$genre_1 = factor(df$genre_1)

# df = df[df$genre_count!=0,]

################################################################################
#COSINE SIMILARITY

cos_sim_df = read.csv("cos_sim.csv")



df = df[!duplicated(df),]
cos_sim_df = cos_sim_df[!duplicated(cos_sim_df),]

colnames(cos_sim_df)[1] = "app_id"

for(id in df$app_id){
  if(id %in% cos_sim_df$app_id){}
  else{print(id)}
}

df = df[df$app_id != 592450 & df$app_id != 562340 & df$app_id != 608000 &
          df$app_id != 453310 & df$app_id != 562340 & df$app_id != 608000,]


for(id in df$app_id){
  if(id %in% cos_sim_df$app_id){}
  else{print(id)}
}

for(i in 1:length(df$app_id)){
  print(df$app_id[i])
  print(cos_sim_df$app_id[i])
}


data = merge(df,cos_sim_df,by="app_id", all=F)


df$positivity = df$pos_rev_num / (df$pos_rev_num + df$neg_rev_num)

#### USING JACCARD SIMILARITY BETWEEN t0 and t1 tag vectors

model0 = lm(jac_sim ~ price_x +
                      positivity +
                      owners_x +
                      genre_1, data=data)




model1 = lm(jac_sim ~ price_x +
                      positivity +
                      owners_x +
                      genre_1 +
                      cos_sim, data=data)


require(stargazer)
stargazer(model0,model1,type="text")

#### USING COUNT OF t1 tags NOT IN t0 TAGS

# model0 = lm(novel_tags_count ~ price_x +
#               positivity +
#               owners_x +
#               genre_1, data=data)

model0 = lm(novel_tags_count ~ price_x +
              positivity +
              owners_x, data=data)

model1 = lm(novel_tags_count ~ price_x +
              positivity +
              owners_x +
              #genre_1 +
              cos_sim, data=data)

require(stargazer)
stargazer(model0,model1,type="text")

#### CONFUSION (ENTROPY OF t1 TAGS) #####

# model0 = lm(t2_entropy ~ price_x +
#               positivity +
#               owners_x +
#               genre_1, data=data)

model0 = lm(t2_entropy ~ price_x +
              positivity +
              owners_x, data=data)


model1 = lm(t2_entropy ~ price_x +
              positivity +
              owners_x +
              #genre_1 +
              cos_sim, data=data)

require(stargazer)
stargazer(model0,model1,type="text")













###############################################################################
#### CONSENSUS

# df$positivity = df$pos_rev_num / (df$pos_rev_num + df$neg_rev_num) 
 
#### USING JACCARD SIMILARITY BETWEEN t0 and t1 tag vectors

# model0 = lm(jac_sim ~ price_x +
#                       positivity +
#                       owners_x + 
#                       genre_1, data=df)
# 

# summary(model0)

# model1 = lm(jac_sim ~ price_x +
#                       positivity +
#                       owners_x +
#                       genre_1 +
#                       genre_count, data=df)

# summary(model1)


#### USING COUNT OF t1 tags NOT IN t0 TAGS


model0 = lm(novel_tags_count ~ price_x +
              positivity +
              owners_x +
              genre_1, data=df)

# summary(model0)

model1 = lm(novel_tags_count ~ price_x +
              positivity +
              owners_x +
              genre_1 +
              genre_count, data=df)

summary(model1)                             ## THIS MODEL IS BEST


require(stargazer)
stargazer(model0,model1,type="text")
# stargazer(model0,model1,type="html", out = "consensus_models.htm")


###############################################################################
#### CONFUSION #####

model0 = lm(t2_entropy ~ price_x +
              positivity +
              owners_x +
              genre_1, data=df)

# summary(model0)

model1 = lm(t2_entropy ~ price_x +
              positivity +
              owners_x +
              genre_1 +
              genre_count, data=df)

# summary(model1)

require(stargazer)
stargazer(model0,model1,type="text")
# stargazer(model0,model1,type="html", out = "confusion_models.htm")





###############################################################################
#### TOBIT REGRESSION MODELS ####

#### CONSENSUS ####

# install.packages("censReg")
require(censReg)

model0 = censReg(novel_tags_count ~ price_x +
                positivity
                #owners_x
                #genre_1
                , right=12, left=0, data=df)

# library(psych)
# describe(df)


model1 = censReg(novel_tags_count ~ price_x +
                positivity +
                #owners_x +
                #genre_1 +
                genre_count
                , data=df)

stargazer(model0, model1,type="text")


#### CONFUSION ####

model0 = censReg(t2_entropy ~ price_x +
              positivity +
              owners_x +
              genre_1, data=df)

# summary(model0)

model1 = censReg(t2_entropy ~ price_x +
              positivity +
              #owners_x +
              #genre_1 +
              genre_count, data=df)

summary(model1)

require(stargazer)
stargazer(model0,model1,type="text")
# stargazer(model0,model1,type="html", out = "confusion_models.htm")

install.packages("DescTools")
library(DescTools)

pseudoR2 <- function(obj) 1 - as.vector(logLik(obj)/logLik(update(obj, . ~ 1)))
pseudoR2(model1)



################################################################################
################################################################################
################################################################################
################################################################################
# MODELS USING KJ TYPICALITY

setwd("C:/Users/gaoan/Desktop/steamspyanalyzer")
rm(list=ls())

df = read.csv("step12_result.csv")
df$positivity = df$pos_rev_num / (df$pos_rev_num + df$neg_rev_num)
# colnames(df[,1:120])

df = df[,c('app_id','price_x', 'positivity', 'owners_x', 'followers_scrap', 'genre_1','num_genres',
           'jac_sim', 'novel_tags_count','t2_entropy', 'genre_count')]

df$genre_1 = factor(df$genre_1)

typ = read.csv("typicality.csv")

colnames(typ)[1] = "app_id"


df = df[!duplicated(df),]
typ = typ[!duplicated(typ),]


bad_ids = vector()

for(id in df$app_id){
  if(id %in% typ$app_id){
  # print(id)
  }
  else{bad_ids = c(bad_ids,id)}
}

print(bad_ids)                              #624460 562340 533970 652150

df = df[!(df$app_id %in% bad_ids),]

mdf = merge(df,typ, by="app_id", all=F)

bad_ids = vector()

for(id in typ$app_id){
  if(id %in% df$app_id){
    #print(id)
  }
  else{bad_ids = c(bad_ids,id)}
}

print(bad_ids)

mgd = merge(df,typ,by="app_id", all=F)

mgd = mgd[!duplicated(mgd$app_id),]

############################################
#MODELS


#### USING COUNT OF t1 tags NOT IN t0 TAGS
model0 = lm(novel_tags_count ~ price_x +
              positivity +
              owners_x+
              num_genres, data=mgd)

# summary(model0)

model1 = lm(novel_tags_count ~ price_x +
              positivity +
              owners_x +
              num_genres +
              TYPICALITY, data=mgd)

# summary(model1)                             ## THIS MODEL IS BEST


require(stargazer)
stargazer(model0,model1,type="text")


## USING JACCARD SIMILARITY BTW t0 AND t1 TAGS

require(censReg)

model0 = lm(novel_tags_count ~ price_x +
                      positivity +
                      owners_x+
                      followers_scrap +
                      genre_1, data=mgd)


# summary(model0)




model1 = lm(novel_tags_count ~ price_x +
                      positivity +
                      owners_x +
                      followers_scrap +
                      genre_1 +
                      TYPICALITY, data=mgd)

# summary(model1)
require(stargazer)
stargazer(model0,model1,type="text")








###########################################
#### CONFUSION #####

model0 = lm(t2_entropy ~ price_x +
              positivity +
              owners_x, data=mgd)

# summary(model0)

model1 = lm(t2_entropy ~ price_x +
              positivity +
              owners_x +
              TYPICALITY, data=mgd)

# summary(model1)

require(stargazer)
stargazer(model0,model1,type="text")
# stargazer(model0,model1,type="html", out = "confusion_models.htm")





str(mgd)

summary(mgd$TYPICALITY)

mgd[mgd$TYPICALITY>0.4,]

mgd = mgd[mgd$genre_1!="Education",]


######################################################################
# TRY MODELS AFTER REMOVING EDUCATION






model0 = lm(novel_tags_count ~ price_x +
              positivity +
              owners_x+
              followers_scrap +
              genre_1, data=mgd)


# summary(model0)




model1 = lm(novel_tags_count ~ price_x +
              positivity +
              owners_x +
              followers_scrap +
              genre_1 +
              TYPICALITY, data=mgd)

# summary(model1)
require(stargazer)
stargazer(model0,model1,type="text")


plot(mgd$owners,mgd$novel_tags_count)

table(mgd$jac_sim)



#####################################################

# CHECK ENTROPY CALCULATION

setwd("C:/Users/gaoan/Desktop/steamspyanalyzer")
rm(list=ls())

orig_df =  read.csv("step12_result.csv")

orig_df[orig_df$app_id == 327690, "t2_entropy"]

df = orig_df

colnames(df)[116:540] # t1 tags
colnames(df)[543:967] # t2 tags

t2_tags_vec = as.vector(t(df[df$app_id == 327690,543:967]))

t2_tags_vec = t2_tags_vec[ t2_tags_vec != 0 ] 

# tot_tags = sum(t2_tags_vec)
# t2_prob_vec = t2_tags_vec / tot_tags

freqs = freqs.empirical(t2_tags_vec)

install.packages("entropy")
require("entropy")

entropy.empirical(t2_prob_vec, unit="log2")


?entropy.empirical













