library(tidyverse)
plot_optimal <- function(title=""){
  # Category properties #
  jnd_min <- 0
  jnd_max <- 22.5
  jnds_1 <- seq(0,jnd_max, .001)
  in_mean_jnds <- jnd_max/2
  in_sd_jnds <- jnd_max/6
  
  # normalizing constant
  in_nc <- pnorm(jnd_max, in_mean_jnds, in_sd_jnds)-pnorm(jnd_min, in_mean_jnds, in_sd_jnds)
  
  in_category <- tibble(
    distribution="in",
    jnds=jnds_1,
    dens=dnorm(jnds_1, in_mean_jnds, in_sd_jnds)/in_nc
  )
  
  out_category <- tibble(
    distribution="out",
    jnds=jnds_1,
    dens=dunif(jnds_1, jnd_min, jnd_max)
  )
  
  # Combine category/non-category distributions
  all_category <- full_join(in_category, out_category) %>%
    group_by(jnds) %>%
    mutate(total=sum(dens)) %>%
    ungroup() %>%
    mutate(optimal=dens/total)
  
  ggplot(filter(all_category, distribution=="in"),aes(jnds, optimal))+
    geom_line(col="darkgreen")+
    scale_y_continuous(limits=c(0,1,.2))+
    labs(y="Optimal P(In Category)",
         x="JNDs from minimum stimulus")+
    ggthemes::theme_few()
}

# Plot Category/Non-Category distributions
plot_categories_psych <- function(title=""){
  # Category properties #
  jnd_min <- 0
  jnd_max <- 22.5
  jnds_1 <- seq(0,jnd_max, .001)
  in_mean_jnds <- jnd_max/2
  in_sd_jnds <- jnd_max/6
  
  # normalizing constant
  in_nc <- pnorm(jnd_max, in_mean_jnds, in_sd_jnds)-pnorm(jnd_min, in_mean_jnds, in_sd_jnds)
  
  in_category <- tibble(
    distribution="in",
    jnds=jnds_1,
    dens=dnorm(jnds_1, in_mean_jnds, in_sd_jnds)/in_nc
  )
  
  out_category <- tibble(
    distribution="out",
    jnds=jnds_1,
    dens=dunif(jnds_1, jnd_min, jnd_max)
  )
  
  # Combine category/non-category distributions
  all_category <- invisible(full_join(in_category, out_category)) %>%
    group_by(jnds) %>%
    mutate(total=sum(dens)) %>%
    ungroup() %>%
    mutate(optimal=dens/total)
  
  ggplot(all_category, aes(jnds, dens))+
  geom_path(aes(col=distribution))+
  geom_vline(xintercept = in_mean_jnds,linetype="dashed",alpha=.75)+
  labs(
    x="JNDs from minimum stimulus",
    y="Density",
    title=title,
  )+
  ggthemes::theme_few()+
  scale_color_manual(labels=c('In Category','Out of Category'),
                     values=c('dark green','red'),
                     name='Category')+
  theme(
    plot.title = element_text(hjust=0.5)
  )
}










