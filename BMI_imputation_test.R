library(mice)

ex_df <- all_df5[,c("ID","sex_mf","APOEe4","age_base","BMI_base",
                                              "alc_base_WHO2","smoking_base",
                                              "gfap_base","nfl_base","ptau_base")]

hist(ex_df$BMI_base)
sum(is.na(ex_df$BMI_base)) # only 4 missing
ex_df <- ex_df[!is.na(ex_df$BMI_base)]

# generate missing BMI values at random
prop_missing <- 0.25 # proportion of missing data to create

train_test_split <- runif(dim(ex_df)[1])>prop_missing
train_df <- ex_df[train_test_split==T,]
test_df <- ex_df[train_test_split==F,]
test_df$BMI_base <- NA
tt_df <- rbind(train_df,test_df) # rebind them

imp.test <- complete(mice(tt_df,seed=0,method="rf"))

output <- data.frame(
  datat=c(rep("real",dim(test_df)[1]),rep("imputed",dim(test_df)[1])),
  BMI=c(ex_df[train_test_split==F,]$BMI_base,imp.test[train_test_split==F,]$BMI_base))
 
# by density it looks great
ggplot(data=output,aes(x=BMI,colour=datat))+geom_density()

# when you plot the actual predictions, not so much
ggplot()+
  geom_point(aes(x=output[output$datat=="real",]$BMI,
                 y=output[output$datat=="imputed",]$BMI))+
  labs(x="Real BMI",y="Imputed BMI")+
  theme_bw()
