

### LOAD LIBRARIES 
library(haven)
library(sas7bdat)
library(dplyr)
library(ggplot2)
library(MatchIt)
library(RBesT)
library(matrixStats)

##################################################################
# Function for DATA PREPARATION STEP
getmydata <- function( path="" ){
  
  setwd(path)
  
  av_dm<-read_xpt("dm.xpt")
  av_vs<-read_xpt("vs.xpt")
  av_qs<-read_xpt("qs.xpt")
  av_su<-read_xpt("su.xpt")
  
  ## Extract variables of interest
  av<-av_dm[c("USUBJID", "SUBJID", "SITEID", "AGE", "SEX",
              "RACE", "ARM")]
  
  subjid<-av_dm$USUBJID
  
  AV_VS_SYSBP<-av_vs[which(av_vs$VISIT=="SCREENING DAY -1" &
                             av_vs$VSTESTCD=="SYSBP"), 
                     c("USUBJID", "VSTESTCD", "VSSTRESN")]
  
  AV_VS_DIABP<-av_vs[which(av_vs$VISIT=="SCREENING DAY -1" & 
                             av_vs$VSTESTCD=="DIABP"), 
                     c("USUBJID", "VSTESTCD", "VSSTRESN")]
  
  AV_VS_HEIGHT<-av_vs[which(av_vs$VSTESTCD=="HEIGHT"), 
                      c("USUBJID", "VSTESTCD", "VSSTRESN")]
  
  
  if(grepl("NCT01077700", path, fixed=TRUE)){
    # M10503-25223-9979391 did not have weight data for day -1,
    # so their weight at visit before baseline timepoint is taken
    AV_VS_WEIGHT<-av_vs[which((av_vs$VISIT=="SCREENING DAY -1" &
                                 av_vs$VSTESTCD=="WEIGHT") | 
                                (av_vs$USUBJID=="M10503-25223-9979391" & 
                                   av_vs$VSTESTCD=="WEIGHT" & 
                                   av_vs$VISIT=="SCREENING VISIT 2")), 
                        c("USUBJID", "VSTESTCD", "VSSTRESN")]
  }else{
    AV_VS_WEIGHT<-av_vs[which(av_vs$VISIT=="SCREENING DAY -1" & 
                                av_vs$VSTESTCD=="WEIGHT"), 
                        c("USUBJID", "VSTESTCD", "VSSTRESN")]
    
  }
  
  if(grepl("NCT01077700", path, fixed=TRUE)){
    # M10503-25053-9970297 did not have substance use data for day 
    # -1, so data from the next visit after start of treatment is 
    # used. 
    AV_SU_TOBACCO<-av_su[which((av_su$VISIT=="SCREENING DAY -1" & 
                                  (av_su$SUTRT=="CIGARETTES USAGE" | 
                                     av_su$SUTRT=="TOBACCO USAGE" | 
                                     av_su$SUTRT=="CHEWING TOBACCO")) | 
                                 (av_su$USUBJID=="M10503-25053-9970297" & 
                                    av_su$SUTRT=="CIGARETTES USAGE" & 
                                    av_su$VISIT=="DAY7")), 
                         c("USUBJID", "SUTRT", "SUDOSE")]
  }else if(grepl("NCT01678755", path, fixed=TRUE)){
    AV_SU_TOBACCO<-av_su[which((av_su$VISIT=="SCREENING DAY -1" & 
                                  (av_su$SUTRT=="CIGARETTES USAGE" | 
                                     av_su$SUTRT=="TOBACCO USAGE"| 
                                     av_su$SUTRT=="CHEWING TOBACCO"))), 
                         c("USUBJID", "SUTRT", "SUDOSE")]
    # remove NA values
    AV_SU_TOBACCO <- AV_SU_TOBACCO[which(AV_SU_TOBACCO$SUDOSE>0),]
  }else{
    AV_SU_TOBACCO<-av_su[which((av_su$VISIT=="SCREENING DAY -1" & 
                                  (av_su$SUTRT=="CIGARETTES USAGE" | 
                                     av_su$SUTRT=="TOBACCO USAGE"| 
                                     av_su$SUTRT=="CHEWING TOBACCO"))), 
                         c("USUBJID", "SUTRT", "SUDOSE")]
    
  }
  AV_SU_TOBACCO$tobacco<-ifelse(AV_SU_TOBACCO$SUDOSE>0, yes=1, 
                                no=0)
  
  if(grepl("NCT01077700", path, fixed=TRUE)){
    AV_SU_ALCOHOL<-av_su[which(av_su$VISIT=="SCREENING DAY -1" & 
                                 av_su$SUTRT=="ALCOHOL USAGE" | 
                                 (av_su$USUBJID=="M10503-25053-9970297" & 
                                    av_su$SUTRT=="ALCOHOL USAGE" & 
                                    av_su$VISIT=="DAY7")), 
                         c("USUBJID", "SUTRT", "SUDOSE")]
  }else{
    AV_SU_ALCOHOL<-av_su[which(av_su$VISIT=="SCREENING DAY -1" & 
                                 av_su$SUTRT=="ALCOHOL USAGE"), 
                         c("USUBJID", "SUTRT", "SUDOSE")]
  }
  AV_SU_ALCOHOL$alcohol<-ifelse(AV_SU_ALCOHOL$SUDOSE>0, yes=1, 
                                no=0)
  
  ##Check that order of subjects is the same for data from each 
  ##domain
  check1 <- sum(AV_VS_DIABP$USUBJID==av$USUBJID)
  check2 <- sum(AV_VS_DIABP$USUBJID==AV_VS_SYSBP$USUBJID)
  check3 <- sum(AV_VS_DIABP$USUBJID==AV_VS_HEIGHT$USUBJID)
  check4 <- sum(AV_VS_DIABP$USUBJID==AV_SU_ALCOHOL$USUBJID)
  check5 <- sum(AV_VS_DIABP$USUBJID==AV_SU_TOBACCO$USUBJID)
  
  ##Combine data for all variables of interest into one data frame
  av$SYSBP<-AV_VS_SYSBP$VSSTRESN
  av$DIABP<-AV_VS_DIABP$VSSTRESN
  av$HEIGHT<-AV_VS_HEIGHT$VSSTRESN
  av$WEIGHT<-AV_VS_WEIGHT$VSSTRESN
  av$ALCOHOL<-AV_SU_ALCOHOL$alcohol
  av$TOBACCO<-AV_SU_TOBACCO$tobacco
  
  if(grepl("NCT01077700", path, fixed=TRUE)){
    AV_QS_PANSS_BASELINE<-
      av_qs[which(av_qs$VISIT=="SCREENING DAY -1" &
                    av_qs$QSCAT=="POSITIVE AND NEGATIVE SYNDROME SCALE (PANSS)" & 
                    av_qs$QSTEST=="PANSS1-Total Score"), 
            c("USUBJID", "QSCAT", "QSTESTCD", "QSSTRESN")]
    #subjid[!(subjid %in% AV700_QS_PANSS$USUBJID)] 
    # M10503-23946-9979030 does not have PANSS score on day -1
  }else{
    AV_QS_PANSS_BASELINE<-
      av_qs[which(av_qs$VISIT=="SCREENING DAY -1" & 
                    av_qs$QSCAT=="POSITIVE AND NEGATIVE SYNDROME SCALE (PANSS)"& 
                    av_qs$QSTEST=="PANSS1-Total Score"), 
            c("USUBJID", "QSCAT", "QSTESTCD", "QSSTRESN")]
  }
  AV_QS_PANSS_BASELINE$panss_baseline<-AV_QS_PANSS_BASELINE$QSSTRESN
  
  if(grepl("NCT01077700", path, fixed=TRUE)){
    AV_QS_PANSS_END<-
      av_qs[which(av_qs$VISIT=="DAY84" & 
                    av_qs$QSCAT=="POSITIVE AND NEGATIVE SYNDROME SCALE (PANSS)" & 
                    av_qs$QSTEST=="PANSS1-Total Score"), 
            c("USUBJID", "QSCAT", "QSTESTCD", "QSSTRESN")]
  }else if(grepl("NCT01095562", path, fixed=TRUE)){
    AV_QS_PANSS_END<-
      av_qs[which(av_qs$VISIT=="DAY 84" & 
                    av_qs$QSCAT=="POSITIVE AND NEGATIVE SYNDROME SCALE (PANSS)" & 
                    av_qs$QSTEST=="PANSS1-Total Score"), 
            c("USUBJID", "QSCAT", "QSTESTCD", "QSSTRESN")]
  }else{
    AV_QS_PANSS_END<-
      av_qs[which(av_qs$VISIT=="WEEK 12" & 
                    av_qs$QSCAT=="POSITIVE AND NEGATIVE SYNDROME SCALE (PANSS)"& 
                    av_qs$QSTEST=="PANSS1-Total Score"), 
            c("USUBJID", "QSCAT", "QSTESTCD", "QSSTRESN")]
  }
  AV_QS_PANSS_END$panss_week12<-AV_QS_PANSS_END$QSSTRESN
  
  base<-AV_QS_PANSS_BASELINE[c("USUBJID", "panss_baseline")]
  end<-AV_QS_PANSS_END[c("USUBJID", "panss_week12")]
  
  AV_QS_OUTCOME<-left_join(x=base, y=end, by="USUBJID")
  
  av<-left_join(x=av, y=AV_QS_OUTCOME, by="USUBJID")
  av$Z<-0
  av$treatment<-ifelse(av$ARM=="PLACEBO", yes=0, no=1)
  av$Y<-av$panss_week12-av$panss_baseline
  if(grepl("NCT01077700", path, fixed=TRUE)){
    av$Study<- -1
    av$NCT<- "NCT01077700"
  }else if(grepl("NCT01095562", path, fixed=TRUE)){
    av$Study<-1
    av$NCT<- "NCT01095562"
  }else if(grepl("NCT01655680", path, fixed=TRUE)){
    av$Study<-2
    av$NCT<- "NCT01655680"
  }else if(grepl("NCT01678755", path, fixed=TRUE)){
    av$Study<-3
    av$NCT<-"NCT01678755"
  }
  
  return(list(av, check1, check2, check3, check4, check5))
  
}

##################################################################
# Functions for PS-DISTR. PLOT
est_PS_plot<-function(group=numeric(), ps=numeric(), title="", 
                      title.size=12, xlab="Propensity score"){
  df<-data.frame(group=vector(length=length(group)), ps=ps)
  for(i in 1:length(group)){
    if(group[i]==1){
      df$group[i]<-"NCT01077700"
    }else{
      df$group[i]<-"External"
    }
  }
  ggplot(df, aes(x=ps, fill=group, color=group)) + 
    geom_histogram(aes(y=..density..), position="identity", 
                   alpha=0.5, bins=40) + 
    scale_color_manual(values=c("darkred", "royalblue"), 
                       breaks=c("NCT01077700", "External")) + 
    scale_fill_manual(values = c("darkred", "royalblue"), 
                      breaks=c("NCT01077700", "External")) + 
    labs(title = title, x=xlab, y="Density") + 
    theme(plot.title = element_text(hjust=0.5, size=title.size)) + 
    theme_bw() + coord_cartesian(ylim = c(0, 9))
}
est_PS_plot2<-function(group=numeric(), ps=numeric(), 
                       title="", title.size=12, 
                       xlab="Propensity score", 
                       cutpoints=numeric()){
  df<-data.frame(group=vector(length=length(group)), ps=ps)
  for(i in 1:length(group)){
    if(group[i]==1){
      df$group[i]<-"NCT01077700"
    }else{
      df$group[i]<-"External"
    }
  }
  ggplot(df, aes(x=ps, fill=group, color=group)) + 
    geom_histogram(aes(y=..density..), 
                   position="identity", alpha=0.5, bins=40) +
    scale_color_manual(values=c("darkred", "royalblue"), 
                       breaks=c("NCT01077700", "External")) +
    scale_fill_manual(values = c("darkred", "royalblue"),
                      breaks=c("NCT01077700", "External")) + 
    labs(title = title, x=xlab, y="Density") + 
    theme(plot.title = element_text(hjust=0.5, size=title.size)) + 
    theme_bw() + geom_vline(xintercept = cutpoints) + 
    coord_cartesian(ylim = c(0, 9))                      
}

##################################################################
# Function for PSMATCH
psmatch <- function(data = as.data.frame()){
  
  df <- data
  
  # logit(PS) estimation by using ALL (current and historical) data
  df$logitPS <- MatchIt::matchit(formula = Z ~ AGE + male  + race 
                                 + SYSBP + DIABP + HEIGHT + WEIGHT 
                                 + TOBACCO,
                                 data = df, method = NULL,
                                 distance = "glm",
                                 link = "linear.logit")$distance
  
  # Now ONLY Matching, match historical control (Z=0, T=0) to
  # current control (Z=1, T=0) patients,
  # use precalculated logit(PS)
  # discard = "control" removes historical unmatched patients
  # since Z=0 contains historical data
  cd.out <- MatchIt::matchit(formula = Z ~ AGE + male  + race 
                             + SYSBP + DIABP + HEIGHT + WEIGHT 
                             + TOBACCO,
                             data = df, method = "nearest",
                             distance = df$logitPS,
                             discard = "control", ratio=1,
                             caliper = 0.2,
                             reestimate = FALSE)
  mdata <- MatchIt::match.data(cd.out, group = "all",
                               distance = "logit_PS")
  
  TE_psmatch <- with(data = mdata[mdata$treatment == 0,],
                     mean(Y))
  #mean of outcome of current & historical matched placebo patients
  
  matched <- nrow(mdata[mdata$Z==0 & mdata$treatment == 0, ])
  #historical (control)
  
  var_psmatch <- with(data = mdata[mdata$treatment == 0, ], var(Y)) 
  #var of outcome of current & historical matched placebo patients
  
  t <- qt(0.975, nrow(mdata)-1) 
  
  lower95_psmatch<-
    TE_psmatch-t*(sqrt(var_psmatch)/sqrt(nrow(mdata)))
  upper95_psmatch<-
    TE_psmatch+t*(sqrt(var_psmatch)/sqrt(nrow(mdata)))
  
  # just for the plot
  # PS before matching
  ps <- MatchIt::matchit(formula = Z ~ AGE + male  + race 
                         + SYSBP + DIABP + HEIGHT + WEIGHT 
                         + TOBACCO,
                         data = df, method = NULL,
                         distance = "glm",
                         link = "logit")$distance
  
  # PS after matching
  ps_matched <- MatchIt::matchit(formula = Z ~ AGE + male  + race 
                                 + SYSBP + DIABP + HEIGHT + WEIGHT 
                                 + TOBACCO,
                                 data = mdata, method = NULL,
                                 distance = "glm",
                                 link = "logit")$distance
  
  return(list(TE_psmatch=TE_psmatch,
              matched=matched, 
              CI_psmatch=c(lower95_psmatch,upper95_psmatch),
              sd=sqrt(var_psmatch),
              a=as.data.frame(cbind(df$Z , ps)), 
              b=as.data.frame(cbind(mdata$Z , ps_matched))))
  
}

# Function for DBB
dbb <- function(data = as.data.frame()){
  
  combined = data
  
  study <-unique(combined[which(combined$Study>0),]$Study) 
  ## vector of studies observed
  
  Y_bar<-numeric()
  sigma<-numeric()
  n<-numeric()
  se<-numeric()
  
  for(i in 1:length(study)){
    
    Y_bar <- c(Y_bar,mean(combined[which(combined$Z == 0 
                                         & combined$Study == i), ]$Y))
    n     <- c(n,sum(combined$Z == 0 & combined$Study == i))
    
    res <- lm(Y ~ AGE + male  + race + SYSBP + DIABP + HEIGHT + 
                WEIGHT + TOBACCO, 
              data = combined[which(combined$Z == 0 
                                    & combined$Study == i), ])
    RSS <- c(crossprod(res$residuals))
    sigma <- c(sigma,  sqrt( RSS / res$df.residual )) #RSE
  }
  
  se    <- sigma / sqrt(n)
  
  sumstat <- t(as.data.frame(rbind(y = Y_bar, y.sigma = sigma, n, 
                                   y.se=se, Study=study)))
  sumstat_dbb <- as.data.frame(sumstat)
  
  gmap <- 
    RBesT::gMAP(cbind(y, y.se) ~ 1 | Study, 
                data = sumstat_dbb,
                weight = n, family = gaussian, 
                beta.prior = cbind(0,100*mean(sumstat_dbb$y.sigma)),
                tau.dist = "HalfNormal", 
                tau.prior = cbind(0,mean(sumstat_dbb$y.sigma) / 8), 
                iter=25000, warmup=5000, chains = 1, thin=1) 
  #moderate level of between trial het. assumed
  map              <- RBesT::automixfit(gmap)
  robust_map_prior_dbb <- RBesT::robustify(map, weight = 0.2)
  
  # prior ESS should be <= 60, if not, then the sd of prior 
  #components get inflated
  priorESS_dbb <- RBesT::ess(robust_map_prior_dbb, "elir")
  ip <- 1 # inflation parameter; neutral
  while(priorESS_dbb > 60){
    ip <- ip + 0.01
    ncol <- ncol(robust_map_prior_dbb)
    robust_map_prior_dbb[3,1:ncol] <- 
      robust_map_prior_dbb[3,1:ncol]*ip
    priorESS_dbb <- RBesT::ess(robust_map_prior_dbb, "elir")
  }
  
  #calculate posterior distribution
  posterior_control_dbb <- 
    RBesT::postmix(robust_map_prior_dbb, 
                   data = combined[which(combined$Z == 1 
                                         & combined$treatment == 0), ]$Y)
  
  ncc <- nrow(combined[which(combined$Z == 1 & 
                               combined$treatment == 0), ])
  #number of current control patients
  posterior_ESS_adj <- 
    RBesT::ess(posterior_control_dbb, "elir") - ncc
  
  
  return(list(sumstat_dbb = sumstat_dbb,
              robust_map_prior_dbb = robust_map_prior_dbb,
              priorESS_dbb = priorESS_dbb,
              ip= ip,
              posterior_control_dbb = posterior_control_dbb,
              posterior_ESS_adj = posterior_ESS_adj))
  
}

##################################################################
# Functions for PSMAP-APPROACH
## Estimate propensity score via logistic regression
ps_logistic<-function(formula=as.formula(""), data=data.frame()){
  # formula should be entered as one would do for glm
  model<-glm(formula=formula, family = binomial(link = "logit"), 
             data=data)
  ptreat<-predict(model, type="response")
  return(ptreat)
}

##Calculates the overlapping coefficient between two distributions 
metric_ovl <- function(cov0, cov1) { 
  # cov0 is a numeric vector of propensity scores for external 
  # study subjects
  # cov1 is a numeric vector of propensity scores for current 
  # study subjects
  cov <- c(cov0, cov1)
  if (length(unique(cov)) <= 10) {
    all_x <- c(rep(0, length(cov0)), rep(1, length(cov1)))
    pt    <- apply(prop.table(table(cov, all_x), 2), 1, min)
    
    return(sum(pt))
  }
  
  mn <- min(cov); 
  mx <- max(cov);
  
  f1 <- approxfun(density(cov1, from = mn, to = mx, bw = "nrd")); 
  #density computes kernel density estimates; from and to indicate
  #the left and right-most points of the grid at which the density 
  # is to be estimated
  f0 <- approxfun(density(cov0, from = mn, to = mx, bw = "nrd")); 
  #approxfun performs linear interpolation of the given data points
  
  fn <- function(x)
    pmin(f1(x), f0(x))
  
  s <- try(integrate(fn, lower = mn, upper = mx,
                     subdivisions = 500)$value)
  
  ifelse(inherits(s, "try-error"), NA, s)
  # the overlapping coefficient value is returned
}

## Stratifies the data based on quantiles of the esitmated PS of 
#current study subjects such that the number of current subjects
#is the same across strata
stratify<-function(ps=numeric(), group=numeric(), nstrata=5){
  strata<-numeric(length(ps))
  
  q<-quantile(ps[which(group==1)], probs=seq(0, 1, 
                                             length.out=(nstrata+1))) 
  ## Constructs the bounds/cut points for each strata
  
  for(i in 1:length(ps)){
    for(j in 1:(length(q)-1)){
      if(j==1){ ## ensures the current study subject with the 
        #smallest propensity score is included
        if(ps[i] >= q[j] & ps[i] <= q[j+1]){
          strata[i]<-j
        }
      }else{
        if(ps[i] > q[j] & ps[i] <= q[j+1]){
          strata[i]<-j
        }
      }    
    }
  }
  return(list(strata=strata, q=q))
  # strata is a vector indicating which stratum each subject falls
  # in q provides the bounds of the strata
}

## Estimates overlapping coefficients and determines number of 
#current and external study subjects within each stratum
get_ovl<-function(ps=numeric(), group=numeric(), strata=numeric(), 
                  nstrata=5){
  out<-matrix(NA, nrow=nstrata, ncol=3)
  rownames(out)<-as.character(1:nstrata)
  colnames(out)<-c("N0", "N1", "OvlC")
  
  for(i in 1:nstrata){
    n0<-sum(strata==i & group==0)
    n1<-sum(strata==i & group==1)
    ps0<-ps[which(strata==i & group==0)]
    ps1<-ps[which(strata==i & group==1)]
    out[i,]<-c(n0, n1, metric_ovl(cov0=ps0, cov1=ps1))
    ## Each row of out is a stratum; N0 is the number of external 
    #study subjects in each stratum, N1 is the number of current 
    #study subjects in each stratum; OvlC provides the overlapping 
    #coefficient in each stratum
  }
  return(out)
}

#Obtaining summary statistics for each external data sets
#within each stratum
sumstat<-function(stratum=data.frame()){ 
  ##input is the data for one stratum only
  nStu<-unique(stratum[which(stratum$Study>0),]$Study) 
  ## vector of studies observed in the particular stratum 
  Y_bar<-numeric()
  sigma<-numeric()
  n<-numeric()
  Stu<-numeric()
  
  for(i in nStu){ ## We obtain summary statistics for the external 
    #data sources present in this stratum
    if(sum(stratum$Z==0 & stratum$Study==i)==1){ ## Cannot obtain 
      #a sd/se estimate with just one observation
      next
    }
    
    Stu<-c(Stu, i )
    Y_bar<-c(Y_bar, mean(stratum[which(stratum$Z==0 
                                       & stratum$Study==i),]$Y)) 
    ## sample mean for external study i
    n<-c(n, sum(stratum$Z==0 & stratum$Study==i)) 
    ## number of subjects in external study i
    
    if(sum(stratum$Z==0 & stratum$Study==i)<=10){
      sigma<-c(sigma, sd(stratum[which(stratum$Z==0 
                                       & stratum$Study==i),]$Y)) 
      ## sample standard deviation for external study i
    }else{
      res <- lm(Y ~ AGE + male  + race + SYSBP + DIABP + HEIGHT 
                + WEIGHT + TOBACCO, 
                data = stratum[which(stratum$Z == 0 
                                     & stratum$Study == i), ])
      RSS <- c(crossprod(res$residuals))
      sigma <- c(sigma,  sqrt( RSS / res$df.residual ))#RSE
    }
    
  }
  
  return(list(Y_bar=Y_bar, sigma=sigma, n=n, nStu=Stu))
}


## Constructs a MAP prior distribution based on external data
MAPprior<-function(stratum = data.frame(), sumstat, nStu=numeric(),
                   iter=2000, burn = 1000, chains=1, thin=1, k=1, 
                   t=1){
  ## In our proposed approach, a MAP prior is constructed
  ## separately for each stratum
  ## stratum is the data for this stratum
  ## sumstat is the list returned by the sumstat function with 
  ## summary statistics and sample sizes of each external study
  ## nStu is a vector of external study numbers returned by the 
  ## sumstat function
  data<-data.frame(Study = nStu, n=sumstat$n, y=sumstat$Y_bar, 
                   y.se = sumstat$sigma/sqrt(sumstat$n)) 
  ## Format data to input in gMAP function 
  
  ## For continuous outcome 
  map_mcmc<-gMAP(cbind(y, y.se)~1 | Study, data=data, weight = n, 
                 family=gaussian, 
                 beta.prior = cbind(0,100*mean(sumstat$sigma)), 
                 tau.dist="HalfNormal", tau.prior=cbind(0, k*t), 
                 iter=iter, warmup=burn, chains = chains, 
                 thin = thin, cores = 2)
  
  map_normmix<-automixfit(map_mcmc) ## Mixture of normals
  
  map_robust <- robustify (map_normmix, weight=0.2) 
  ## Add robust component to the mixture
  return(list(map_mcmc=map_mcmc, normmix=map_normmix, 
              robust=map_robust))
  ## The original gMAP object, normal mixture approximation, 
  ## and the robust MAP priors are returned
}

# This function is directly copied from the source code, 
#getAnywhere(uisd.default)
uisdd <- function (n, sigma, sigma2 = sigma^2, labels = NULL, 
                   individual = FALSE,
                   ...)
{
  stopifnot(length(n) == length(sigma2), all(is.finite(n)),
            all(is.finite(sigma2)), all(n > 0), all(sigma2 > 0),
            (!individual) | ((length(labels) == 0) 
                             || (length(labels) ==
                                   length(sigma2))))
  if (individual) {
    if (!is.character(labels))
      labels <- as.character(labels)
    result <- sqrt(n * sigma2)
    names(result) <- labels
  }
  else {
    result <- sqrt(sum(n)/sum(1/sigma2))
  }
  return(result)
}

## This approach employs an assumed level of heterogeneity for 
## specifying the hyperprior on tau 
## Could be based on belief regarding how heterogeneous the 
## external data sets are
uisd_t<-function(strata = list(), sumstat.list=list(), nstrata=5, 
                 iter=2000, burn=1000, chains = 1, thin=1, 
                 k=numeric(), hetLevel="small", 
                 ess_tar=numeric()){
  ## strata consists of a list where each element is a data frame 
  ## corresponding to a single stratum
  ## sumstata.list is a list where each element contains the 
  ## summary statistics for one stratum--list returned from sumstat
  ## function
  uisd<-numeric(length = nstrata)
  
  for(i in 1:nstrata){
    uisd[i]<-uisdd(n = sumstat.list[[i]]$n, 
                   sigma = sumstat.list[[i]]$sigma/sqrt(sumstat.list[[i]]$n)) 
    ## calculates the unit information standard deviation 
    ## based on external data in each stratum
  }
  
  if(hetLevel == "small"){
    t<-uisd/32
  }else if(hetLevel == "moderate"){
    t<-uisd/16
  }else if(hetLevel == "substantial"){
    t<-uisd/8
  }else if(hetLevel == "large"){
    t<-uisd/4
  }
  
  MAPpriors<-vector(mode = "list", length = nstrata)
  ESS<-numeric(length=nstrata)
  
  for(i in 1:nstrata){
    temp <- MAPprior(stratum = strata[[i]], 
                     sumstat = sumstat.list[[i]], 
                     nStu = sumstat.list[[i]]$nStu, 
                     iter = iter, burn = burn, chains = chains, 
                     thin=thin, k = k[i], t=t[i]) 
    ## Constructs MAP prior based on the assumed degree
    ## of heterogeneity
    MAPpriors[[i]] <- temp$robust
    ESS[i] <- RBesT::ess(MAPpriors[[i]], "elir") 
    ## Calculates the ESS corresponding to the robust MAP prior 
  }
  
  # sum(prior ESS) should be <= ess_tar, if not, 
  #then the sd of prior components (of all strata) get inflated
  ip <- 1 # inflation parameter; neutral
  while(sum(ESS) > ess_tar){
    ip <- ip + 0.1
    for(i in 1:nstrata){
      temp <- MAPprior(stratum = strata[[i]], 
                       sumstat = sumstat.list[[i]], 
                       nStu = sumstat.list[[i]]$nStu, 
                       iter = iter, burn = burn, chains = chains, 
                       thin=thin, k = k[i], t=t[i])
      MAPpriors[[i]] <- temp$robust
      MAPpriors[[i]][3,1:ncol(MAPpriors[[i]])] <- 
        MAPpriors[[i]][3,1:ncol(MAPpriors[[i]])]*ip
      ESS[i] <- RBesT::ess(MAPpriors[[i]], "elir")
    }
  }
  
  return(list(MAPpriors=MAPpriors, ESS = ESS, t=t, ip=ip))
  
}
## Obtain posterior distributions of the average outcome under 
## control as well as the treatment effect (difference)
strata_post<-function(MAPpriors = list(), nstrata= 5, 
                      curr_controls = data.frame(), 
                      post_draws=1000){
  ## Each element of MAP priors is the robust prior constructed 
  ## from external data in a strata
  ## curr_controls consists of data on control/placebo subjects 
  ## in the current trial or study
  post_control<-vector(mode = "list", length = nstrata) 
  ## List to store posterior distributions  of outcome under 
  ## control for all strata
  ## The following stores the samples/draws from the posterior 
  ## distributions of the mean outcomes for all strata 
  ## (each column is a separate stratum)
  postsamp_control<-matrix(NA, nrow=post_draws, ncol=nstrata)
  colnames(postsamp_control)<-paste0("S", 1:nstrata)
  
  posterior_ESS <- vector(length = nstrata)
  
  for(i in 1:nstrata){
    post_control[[i]] <- 
      postmix(MAPpriors[[i]], 
              data = curr_controls[which(curr_controls$strata==i),]$Y) 
    # Update prior with current control group data to obtain
    # posterior distribution of parameter under control 
    postsamp_control[,i]<-RBesT::rmix(mix = post_control[[i]], 
                                      n=post_draws) 
    ## Sampling from posterior
    posterior_ESS[i] <- RBesT::ess(post_control[[i]], 
                                   method = "elir")
  }
  
  return(list(posterior=post_control, 
              postsamp_control=postsamp_control,
              posterior_ESS))
}


## The following function conducts one implementation of the 
## within-stratum MAP prior approach that specifies the hyperprior
## on tau according an assumed level of heterogeneity; 
## may be used for simulation or data analysis
one_sim_sep_strata_het<-function(data = data.frame(), 
                                 covariates=character(), 
                                 nstrata = 5, 
                                 iter=2000, burnin = 1000, 
                                 chains=1, thin=1, 
                                 heterogeneity="small", 
                                 ess_tar=100){
  
  all<-data
  
  f<-paste0("Z ~ ", covariates[1])
  if(length(covariates)>=2){
    for(i in 2:length(covariates)){
      f<-paste0(f, " + ", covariates[i])
    }
  }
  
  ## Estimates propensity scores using logistic regression
  all$ps<-ps_logistic(formula = as.formula(f), data = all) 
  
  stratifying<-stratify(ps = all$ps, group = all$Z, 
                        nstrata = nstrata)
  
  q <- stratifying$q
  
  all$strata<-stratifying$strata
  
  boundsE1<-quantile(all[which(all$Z==1),]$ps, probs=c(0,1))
  
  # Trim external subjects whose PS are outside range of current 
  # subjects' PS
  subset<-
    all[which(all$ps>= boundsE1[1] & all$ps <= boundsE1[2]),]
  
  tab_OvlC<-get_ovl(ps=subset$ps, group=subset$Z, 
                    strata=subset$strata, nstrata=nstrata)
  
  R<-tab_OvlC[,3]
  r_ref<-median(R)
  k<-r_ref/R
  
  strata.list<-vector(mode = "list", length = nstrata)
  sumstat.list<-vector(mode = "list", length = nstrata)
  
  for(i in 1:nstrata){
    strata.list[[i]]<-subset[which(subset$strata==i),]
    sumstat.list[[i]]<-sumstat(stratum = strata.list[[i]])
  }
  
  MAPt<-uisd_t(strata = strata.list, sumstat.list = sumstat.list, 
               nstrata = nstrata, iter = iter, burn = burnin, 
               chains = chains, thin = thin, k = k, 
               hetLevel = heterogeneity, ess_tar = ess_tar)
  
  totalESS<-tab_OvlC[,2] + MAPt$ESS
  
  weight_strata <- R/sum(R)
  
  posterior<-strata_post(MAPpriors = MAPt$MAPpriors, 
                         nstrata=nstrata, 
                         curr_controls = 
                           subset[which(subset$Z==1 
                                        & subset$treatment==0),],
                         post_draws=10000)
  post <- as.data.frame(posterior[[2]])
  
  weighted <- sweep(post, MARGIN = 2, weight_strata, `*`)
  overall_post <- rowSums(weighted)
  
  CrI <- quantile(overall_post, probs=c(.025, .975))
  
  ncc <- nrow(subset[which(subset$Z == 1 & subset$treatment == 0), ])
  #number of current control patients
  post_ESS_adj <- sum(posterior[[3]]) - ncc
  
  
  return(list(TE=mean(overall_post), CrI=CrI , sd=sd(overall_post),
              posterior=posterior[[1]],
              totalESS=totalESS, R=R, weight_strata=weight_strata,
              n1=tab_OvlC[,2],n0=tab_OvlC[,1], t=MAPt$t, 
              priorESS=MAPt$ESS, ip=MAPt$ip, tab_OvlC=tab_OvlC, 
              q=q, all=all, subset=subset, 
              sumstat.list=sumstat.list, MAP=MAPt$MAPpriors,
              post_ESS_adj=post_ESS_adj))
  
}

##################################################################
######################     MAIN PART      ########################
##################################################################

#### LOAD DATA

#path 1 to 4 are the paths where data is stored
av700_list <- getmydata(path=path1)
av562_list <- getmydata(path=path2)
av680_list <- getmydata(path=path3)
av755_list <- getmydata(path=path4)

av700 <- av700_list[[1]]
av562 <- av562_list[[1]]
av680 <- av680_list[[1]]
av755 <- av755_list[[1]]

mean(av700[which(!is.na(av700$Y)),]$Y) 
#-3.935484, for 62 patients Y available
av700[which(av700$AGE <20 | av700$AGE>55),]$USUBJID 
# 2 patients from current study will later be discarded analysis

external<-rbind(av700, av562, av680, av755) 
## Combine all the external data sets into one data frame
external$Z[which(external$NCT=="NCT01077700")] <- 1 
## NCT01077700 is treated as the current trial
combined <- external
combined<-combined[!is.na(combined$Y),] 
## Discard subjects with missing outcome information
combined$male<-ifelse(combined$SEX=="M", yes=1, no=0)
combined<-combined[which(combined$AGE>=20 & combined$AGE <=55),]
nrow(combined[which(combined$NCT=="NCT01077700" & 
                      combined$treatment==1),]) 
## Number of treated subjects in NCT01077700 trial
nrow(combined[which(combined$NCT=="NCT01077700" & 
                      combined$treatment==0),]) 
## Number of control subjects in NCT01077700 trial
combined<-combined[!(combined$RACE==""),]
nrow(combined)
table(combined$NCT) ## Sample sizes for analysis
combined$race<-vector(length=nrow(combined))
for(i in 1:nrow(combined)){
  if(combined$RACE[i]=="WHITE"){
    combined$race[i]<-"WHITE"
  }else if(combined$RACE[i]=="BLACK OR AFRICAN AMERICAN"){
    combined$race[i]<-"BLACK"
  }else if(combined$RACE[i]=="ASIAN"){
    combined$race[i]<-"ASIAN"
  }else{
    combined$race[i]<-"OTHER"
  }
}

table(combined$race)

##################################################################
######################     APPROACHES     ########################
##################################################################

#### PS-MATCH
temp_psmatch <- psmatch(data = combined)
temp_psmatch$TE_psmatch #-4.08
temp_psmatch$matched #60
temp_psmatch$CI_psmatch #[-5.37,-2.8]
temp_psmatch$sd #7.1

# Plot PS distribution before and after Matching
a <- temp_psmatch$a
colnames(a) <- c("Z","ps")
est_PS_plot(group=a$Z, ps=a$ps, title="NCT01077700 vs 
            External Propensity Scores")
b <- temp_psmatch$b
colnames(b) <- c("Z","ps")
est_PS_plot(group=b$Z, ps=b$ps, title="NCT01077700 vs 
            External Propensity Scores after Matching")  

#### REPRODUCIBILITY
set.seed(84909)
temp<- one_sim_sep_strata_het(data=combined, 
                              covariates=c("AGE", "male", "race", 
                                           "SYSBP", "DIABP", 
                                           "HEIGHT", "WEIGHT",
                                           "TOBACCO"), nstrata=5, 
                              iter = 25000, burnin = 5000, 
                              chains=1, thin = 1, 
                              heterogeneity="moderate", 
                              ess_tar=60) # PSMAP
temp0 <- dbb(data = combined) # DBB

#### DBB
sumstat_dbb           <- temp0$sumstat_dbb
robust_map_prior_dbb  <- temp0$robust_map_prior_dbb
priorESS_dbb          <- temp0$priorESS_dbb
ip                    <- temp0$ip
posterior_ESS_adj_dbb <- temp0$posterior_ESS_adj
posterior_control_dbb <- temp0$posterior_control_dbb

TE_estimate_dbb <- 
  weighted.mean(x=
                  posterior_control_dbb[2,1:ncol(posterior_control_dbb)], 
                w=posterior_control_dbb[1,1:ncol(posterior_control_dbb)])
CrI_dbb <- 
  as.numeric(cbind(summary(posterior_control_dbb)[" 2.5%"],
                   summary(posterior_control_dbb)["97.5%"]))
sd_dbb <- summary(posterior_control_dbb)["sd"]
plot(robust_map_prior_dbb)
plot(posterior_control_dbb)

#### PSMAP 
TE <- temp$TE
sd_psmap <- temp$sd
CrI_psmap <- as.numeric(cbind(temp$CrI[1],temp$CrI[2]))
posterior_ESS_adj_psmap <- temp$post_ESS_adj
posterior_control_strata <- temp$posterior

#Plot PS distribution before and after Trimming
cut_points <- as.numeric(temp$q)
dataset <- temp$all
subset <- temp$subset
est_PS_plot2(group=dataset$Z, ps=dataset$ps,
             title="NCT01077700 vs External Propensity Scores", 
             cutpoints=cut_points)
est_PS_plot2(group=subset$Z, ps=subset$ps, 
             title="NCT01077700 vs External Propensity Scores 
             after trimming external patients", 
             cutpoints=cut_points)

#MAP priors
plot(temp$MAP[[1]])
plot(temp$MAP[[2]])
plot(temp$MAP[[3]])
plot(temp$MAP[[4]])
plot(temp$MAP[[5]])

#Posterior distributions
plot(temp$posterior[[1]])
plot(temp$posterior[[2]])
plot(temp$posterior[[3]])
plot(temp$posterior[[4]])
plot(temp$posterior[[5]])


