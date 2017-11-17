#-------------------------------------------------------------------------------
require(Ckmeans.1d.dp)
x <- c(rnorm(50, mean=-1, sd=0.3), rnorm(50, mean=1, sd=1), rnorm(50, mean=2, sd=0.4))
# Divide x into k clusters, k automatically selected (default: 1~9)
result <- Ckmeans.1d.dp(x)
plot(result)

k <- max(result$cluster)
plot(x, col=result$cluster, pch=result$cluster, cex=1.5,
     main="Optimal univariate clustering with k estimated",
     sub=paste("Number of clusters is estimated to be", k))
abline(h=result$centers, col=1:k, lty="dashed", lwd=2)
legend("topleft", paste("Cluster", 1:k), col=1:k, pch=1:k, cex=1.5, bty="n")


x <- stone_artefacts_only$depth_below_ground_surface

ahist(x, k=max(result$cluster), col="gray",
      lwd=2, lwd.stick=6, col.stick="chocolate")

stone_artefacts_only$cluster <- result$cluster


# only plot one point per artefact (some artefacts have multiple total station points)
library(viridis)
stone_artefacts_only_one <-
  stone_artefacts_only %>%
  group_by(Description, find, cluster) %>%
  dplyr::summarise(Xnew_flipped = mean(Xnew_flipped),
                   depth_below_ground_surface = mean(depth_below_ground_surface))



# determined by plotting row C end levels
row_c <- c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
row_mids <- row_c/2
size = 0.25

p <- ggplot() +
  geom_point(data = stone_artefacts_only_one,
             aes(Xnew_flipped,
                 depth_below_ground_surface,
                 colour = as.character(cluster)),
             size = size) +
  scale_y_reverse(limits = c(3,0)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(breaks = row_c,
                     labels = NULL) +
  xlab("") +
  ylab("Depth below \nground surface (m)") +
  scale_colour_brewer(palette = "Set1",
                      "Cluster") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  coord_equal()

row_c = c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
nums = paste0("B", 6:1)
row_mids <-  row_c[-length(row_c)] + diff(row_c)/2

library(grid)
for(i in 1:length(row_mids)){
  p = p + annotation_custom(grob = textGrob(nums[i], gp=gpar(fontsize=10)),
                            xmin =  row_mids[i],
                            xmax =  row_mids[i],
                            ymin = -8.5,
                            ymax = 2)
}

# Code to override clipping
grid.newpage()
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)
# output to RStudio plot pane, then save as SVG


# compare a null hypothesis
make_null_distribution <- function() {
  x_unif <- runif(n = length(x),
                  min = min(x),
                  max = max(x))

  x_unif_result <-
    Ckmeans.1d.dp(x_unif, k = c(1, 50), estimate.k = "BIC")
  # plot(x_unif_result)

  x_unif_k <- max(x_unif_result$cluster)

  return(x_unif_k)
}

n = 10000
generated_null_distributions <- replicate(make_null_distribution(), n)
hist(generated_null_distributions)

# get a p-value: proportion of simulations with the same or less clusters than observed
p_sim <- sum(generated_null_distributions <= k) / length(generated_null_distributions)


# plot observed with null
library(glue)
obs_null <-
  ggplot(data_frame(g = generated_null_distributions),
         aes(g)) +
  geom_histogram() +
  theme_bw() +
  geom_vline(xintercept =  k,
             colour = "red",
             size = 1.5) +
  xlab(glue("Number of clusters in {n} random distributions")) +
  annotate("text",
           x = k-1.5,
           y = 1500,
           label = "Observed k") +
  annotate("text",
           x = 17,
           y = 1200,
           label = glue("Probability of the observed or\nfewer number of clusters\n is {p_sim}"),
           hjust = 0)

library(cowplot)

plot_grid(obs_null,
          gt,
          ncol = 1,
          align = "hv",
          axis = "lr")

# what does it look like?
stone_artefacts_only$simulated_depths <- x_unif
stone_artefacts_only$simulated_cluster <- x_unif_result$cluster

ggplot() +
  geom_point(data = stone_artefacts_only,
             aes(Xnew_flipped,
                 simulated_depths,
                 colour = as.factor(simulated_cluster))
  ) +
  scale_y_reverse(limits = c(3,0)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(breaks = row_c,
                     labels = NULL) +
  xlab("")



# test of k range, yes we can get more than 9
t1 <- flatten_dbl(map(seq(1,100,5), ~rnorm(n= 10, mean = .x, sd = 0.0001)))
hist(t1, breaks = 100)
(t2 <- Ckmeans.1d.dp(t1, k = c(1,50), estimate.k = "BIC"))
max(t2$cluster)

#----------------------------------------------------------------------------
# mag sus plot over hearth

ms <- readxl::read_excel("D:/My Documents/My UW/Research/1206 M2 excavation/MS data/notebook/hearth_feature_SF53.xlsx")

library(tidyverse)
ms %>%
  rowwise() %>%
  mutate(mean_ms = mean(c(meas1, meas2, meas3), na.rm = TRUE)) %>%
  ggplot(aes(distance,
             mean_ms)) +
  geom_point() +
  geom_line() +
  ylab("Magnetic Susceptibility\nSI units") +
  theme_bw() +
  annotate("rect",
           xmin = 83,
           xmax = 90,
           ymin = -Inf,
           ymax = 300,
           fill = "red",
           alpha=0.5 ) +
  annotate("text",
           x = 73,
           y = 250,
           label = "Hearth\nlocation",
           hjust = 0)

