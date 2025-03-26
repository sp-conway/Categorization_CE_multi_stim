library(tidyverse)
# Plot Category/Non-Category distributions for experiment 1b
plot_categories_psych_1b <- function(title=""){
  # Category properties #
  jnd_min <- 0
  jnd_max <- 22.5
  jnds_1 <- seq(0,jnd_max, .001)
  out_mean_jnds <- jnd_max/2
  out_sd_jnds <- jnd_max/6
  
  # normalizing constant
  out_nc <- pnorm(jnd_max, out_mean_jnds, out_sd_jnds)-pnorm(jnd_min, out_mean_jnds, out_sd_jnds)
  
  out_category <- tibble(
    distribution="out",
    jnds=jnds_1,
    dens=dnorm(jnds_1, out_mean_jnds, out_sd_jnds)/out_nc
  )
  
  in_category <- tibble(
    distribution="in",
    jnds=jnds_1,
    dens=dunif(jnds_1, jnd_min, jnd_max)
  )
  
  # Combine category/non-category distributions
  all_category <- full_join(in_category, out_category) %>%
    group_by(jnds) %>%
    mutate(total=sum(dens)) %>%
    ungroup() %>%
    mutate(optimal=dens/total)
  
  ggplot(all_category, aes(jnds, dens,col=distribution))+
    geom_line(size=8)+
    #geom_vline(xintercept = out_mean_jnds,linetype="dashed",alpha=.75)+
    labs(
      x="JNDs from minimum stimulus",
      y="Density",
      title=title
    )+
    theme_bw()+
    scale_color_manual(labels=c('In Category','Out of Category'),
                       values=c('dark green','red'),
                       name='Category')+
    ggthemes::theme_few()+
    theme(
      plot.title = element_text(hjust=0.5)
    )
}

plot_optimal_1b <- function(ll=F,title=""){
  # Large sequence of jnds
  jnd_min <- 0
  jnd_max <- 22.5
  jnds_1 <- seq(0,jnd_max, .01)
  
  # Category properties
  out_mean_jnds <- jnd_max/2
  out_sd_jnds <- jnd_max/10
  
  # normalizing constant
  out_nc <- pnorm(jnd_max, out_mean_jnds, out_sd_jnds)-pnorm(jnd_min, out_mean_jnds, out_sd_jnds)
  
  in_category <- tibble(
    distribution="in",
    jnds=jnds_1,
    dens=dunif(jnds_1, jnd_min, jnd_max)
  )
  
  out_category <- tibble(
    distribution="out",
    jnds=jnds_1,
    dens=dnorm(jnds_1, out_mean_jnds, out_sd_jnds)/out_nc
  )
  
  # Combine category/non-category distributions
  if(ll){
    all_category <- bind_rows(in_category, out_category) %>%
      pivot_wider(names_from=distribution,
                  values_from=dens) %>%
      mutate(lik=log(`in`/out))
    ggplot(all_category, aes(jnds, lik))+
      geom_line(col="black",alpha=.5)+
      geom_hline(yintercept=0,alpha=.75,linetype="dashed")+
      labs(y="Log Likelihood in category",
           x="JNDs from minimum stimulus",
           title=title)+
      scale_y_continuous(limits=c(-2,12))+
      ggthemes::theme_few()
  }else{
    all_category <- bind_rows(in_category, out_category) %>%
      group_by(jnds) %>%
      mutate(total=sum(dens)) %>%
      ungroup() %>%
      mutate(lik=dens/total)
    ggplot(filter(all_category, distribution=="in"),aes(jnds, lik))+
      geom_line(col="darkgreen")+
      #scale_y_continuous(breaks=c(0,1))+
      labs(y="Optimal P(In Category)",
           x="JNDs from minimum stimulus",
           title=title)+
      ggthemes::theme_few()
  }
}