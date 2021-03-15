
# Funciton loop: each item

top5_txt<- df %>%
  filter(item %in% c("Cigarette butts & filters", 
                     "Plastic packaging food (wrap, packets, containers)",
                     "Plastic wrap non food (bubble wrap etc)",
                     "Plastic film remnants (bits of plastic bag, wrap etc)",
                     "Miscellaneous paper, labels & tickets")) %>% 
  distinct(item)%>%
  pull(item) %>% 
  droplevels()
 
# Purrr
library(purrr)

pred_df = purrr::map_df(top5_txt, function(x){
  
  temp_df = df %>% 
    filter(item == paste0(x)) %>%
    group_by(event_id, asset_id, cycle, date, covid, LGA, LUZ) %>% #relevant variables
    summarise(sum=sum(sum)) %>%
    ungroup() %>%
    filter(cycle %in% paste0("C",1:7)) %>%
    mutate(LGA = as.factor(LGA),
           LUZ = as.factor(LUZ),
           covid = as.factor(covid),
           cycle = droplevels(as.factor(cycle)),
           item = x)
  
 # p1 = ggplot(data=temp_df, aes(x=covid, y=sum+0.5, color=LUZ)) +
 #    geom_boxplot() +
 #    scale_y_log10() 
 
 #create model
 m_temp  <- glmmTMB(sum ~ #Debris count per survey event
                   LUZ * covid + #predicted by land use effected by covid
                   (1|LGA/asset_id) + (1|cycle), #suburb random effect
                   family=nbinom2(), #negative binomial to deal with count data and zeros
                   data = temp_df)

#  Model validation -------------------------------------------------------

 
 # emmeans(m_temp, ~covid|LUZ)
 # emmeans(m_temp, pairwise~covid|LUZ, type="response")
 
 #dharma cigarette
 # simulationOutput_cig <- simulateResiduals(fittedModel = m_cig, plot=T)
 # 
 # plotResiduals(simulationOutput_cig)
 # testUniformity(simulationOutput_cig) #tests if the overall distribution conforms to expectations
 # # testOutliers(simulationOutput_cig) #tests if there are more simulation outliers than expected
 # testDispersion(simulationOutput_cig) #tests if the simulated dispersion is equal to the observed dispersion
 # #testQuantiles(simulationOutput_cig) #fits a quantile regression or residuals against a predictor (default predicted value), and tests of this conforms to the expected quantile
 # testZeroInflation(simulationOutput_cig) #tests if there are more zeros than expected


# Model validation end ----------------------------------------------------

 nd_temp <- expand.grid(covid = unique(temp_df$covid),
                       LUZ = unique(temp_df$LUZ),
                       LGA = NA,
                       asset_id = NA,
                       cycle = NA)
 
 #predict function uses the model and the new df to predict mean values of the response variable.  It predicts a value for each row in the new df.
 pred_temp <- predict(object=m_temp,
                     newdata=nd_temp,
                     se.fit=T,
                     re.form=NA,
                     type="response")
 
 #creating standard error for graphing
 nd_temp$Total <- pred_temp$fit
 nd_temp$SE_upper <- pred_temp$fit + pred_temp$se.fit
 nd_temp$SE_lower <- pred_temp$fit - pred_temp$se.fit
 nd_temp$item = x
 
 return(nd_temp)
 
 })


 #creating plot
 p_all <- ggplot(pred_df, aes(y=Total, x=LUZ)) + 
   geom_bar(stat = "identity",
            aes(fill=covid),
            position = "dodge") +
   # geom_errorbar(aes(ymin=SE_lower, ymax=SE_upper, width=0.2),
   #                stat = "identity") +
   labs(title="Top debris type abundance predictions",
        subtitle="By land use zone and covid restriction",
        x="Covid lockdown status", y="Total debris", tag="") +
   facet_wrap(~item) +
   theme_minimal()
 

 p_all
 
 

