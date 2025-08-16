# Let's write a function for extracting values from a meta-regression

extractor_metareg <- function(model, moderator_name) {
  
  mod.summ <- summary(model)
  # Extract levels of moderator (from rownames)
  mod.levels <- rownames(mod.summ$beta)
  mod.levels <- gsub(paste0(moderator_name),"", mod.levels)
  
  # Need to pass the complete data from the model to be able to calculate k and n for each level of moderator
  mod.data <- model$data
  
  summ.k_n <-  mod.data %>%
    group_by(level= as.character(.data[[moderator_name]])) %>%
    summarise(
      k = n(),  # effect sizes
      n = n_distinct(paper_ID),  # unique papers
      .groups = "drop"
    )
  
  # Now we need a small function to extract the prediction interval per level of the moderator
  # Get prediction intervals for each level
  get_predicted<-function(model,mod.levels)
  {
    # Identity matrix for dummy-coded levels - categorical variables need dummy coding in metafor
    newmods_mat <- diag(length(mod.levels))
    predict.data<- data.frame(predict(model,newmods = newmods_mat))
    predict.data<- predict.data%>%bind_cols(level=mod.levels)%>%select(pi.lb,pi.ub,level)
    return(predict.data)
  }
  
  prediction_interval<-get_predicted(model,mod.levels)
  
  ## Let's get all the heterogeneity metrics too
  ## I am copying the r2_ml and R2_calc function code from orchaRd package
  r2_ml <- function(model, data, boot = NULL) {
    
    if(all(class(model) %in% c("robust.rma", "rma.mv", "rma", "rma.uni")) == FALSE) {stop("Sorry, you need to fit a metafor model of class robust.rma, rma.mv, rma, rma.uni")}
    
    if(any(model$tau2 > 0)) { stop("Sorry. At the moment r2_ml cannot take models with heterogeneous variance.")}
    
    R2 <- R2_calc(model)
    
    if(!is.null(boot)){
      
      if(any(class(model) %in% c("robust.rma")) == TRUE){stop("Sorry, bootstrapping currently doesn't work for robust.rma objects. Please use rma.mv instead.")}
      # Simulate the vector of effect sizes
      sim <- metafor::simulate.rma(model, nsim=boot) # Add try catch here? DN
      
      # Get formula from model object.
      random_formula <- model$random
      mods_formula <- metafor::formula.rma(model, type = "mods") #in case moderators
      vi <- model$vi
      
      pb <- progress::progress_bar$new(total = boot,
                                       format = "Bootstrapping [:bar] :percent ETA: :eta",
                                       show_after = 0)
      # Parametric bootstrap
      R2 <- sapply(sim, function(ysim) {
        # The model
        tmp <- tryCatch(metafor::rma.mv( ysim, vi,
                                         mods = mods_formula,
                                         random = random_formula,
                                         data = data))
        R2s <- R2_calc(tmp)
        pb$tick()
        Sys.sleep(1 / boot)
        return(R2s)
      })
      
      # Summarise the bootstrapped distribution.
      R2 <- data.frame(t(apply(R2, 1, stats::quantile, probs=c(0.5, .025, .975))))
      R2 <-  round(R2, digits = 3)
      colnames(R2) = c("Est.", "2.5%", "97.5%")
    }
    
    return(R2)
    
  }
  
  
  R2_calc <- function(model){
    if(all(class(model) %in% c("robust.rma", "rma.mv", "rma", "rma.uni")) == FALSE) {stop("Sorry, you need to fit a metafor model of class robust.rma, rma.mv, rma, rma.uni")}
    # fixed effect variance
    fix <- stats::var(as.numeric(as.vector(model$b) %*% t(as.matrix(model$X))))
    
    # marginal
    R2m <- fix / (fix + sum(model$sigma2))
    
    # conditional. Need to remove 'residual' variance; assume this is the sigma level with the largest k. Really the only way we can get that to work.
    R2c <- (fix + sum(model$sigma2) - model$sigma2[which(model$s.nlevels.f == max(model$s.nlevels.f))]) /
      (fix + sum(model$sigma2))
    
    R2s <- c(R2_marginal = R2m, R2_conditional = R2c)
    return(R2s)
  }
  
  
  ## Now let's create the dataframe
  tibble(
    moderator = moderator_name,
    level = mod.levels,
    estimate = format(round(mod.summ$beta, 3),nsmall=3),
    ci_lower = format(round(mod.summ$ci.lb, 3),nsmall=3),
    ci_upper = format(round(mod.summ$ci.ub, 3),nsmall=3),
    p_value = format(round(mod.summ$pval, 3),nsmall=3),
    Q = format(round(mod.summ$QE, 2),nsmall=2),
    Q_pval = round(mod.summ$QEp, 5),
    R2_marginal = format(round(r2_ml(model)[1],3),nsmall=3)
  )%>%
    left_join(summ.k_n, by="level")%>%
    left_join(prediction_interval, by= "level")%>%
    mutate(
      pi_lower = format(round(pi.lb,3),nsmall=3),
      pi_upper = format(round(pi.ub,3),nsmall=3),
      CI = paste0("[", ci_lower, ", ",ci_upper, "]"),
      PI = paste0("[", pi_lower, ", ",pi_upper, "]"),
    )%>%
    select(moderator,level,estimate,p_value,CI,PI,k,n,Q,Q_pval,R2_marginal)%>%
    mutate(
      Q_pval = if_else(as.numeric(Q_pval) <0.001, "<0.001",as.character(Q_pval)),
      p_value = if_else(as.numeric(p_value) <0.001, "<0.001",as.character(p_value))
    )
  
  
}
