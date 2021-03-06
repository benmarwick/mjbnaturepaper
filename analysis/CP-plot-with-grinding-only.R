

# same as the published one, but only highlighting axes, axe flakes and grinding against a background of flake/core. This would mean getting rid of ochre and ochred slabs.


# only plot one point per artefact (some artefacts have multiple total station points)
library(tidyverse)
library(viridis)
library(grid)

stone_artefacts_only_one <-
  stone_artefacts_only %>%
  group_by(Description, find) %>%
  dplyr::summarise(Xnew_flipped = mean(Xnew_flipped),
                   depth_below_ground_surface = mean(depth_below_ground_surface))


# determined by plotting row C end levels
row_c <- c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
row_mids <- row_c/2
size = 3

p <- ggplot() +
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "L", ],
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = plasma(6)[1],
             size = size-2.5) +
  # geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "HM", ],
  #            aes(Xnew_flipped,
  #                depth_below_ground_surface),
  #            colour = plasma(6)[2],
  #            size = size-2)  +
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "GS", ],
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = plasma(6)[3],
             size = size-1.5)  +
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "AXE", ],
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = plasma(6)[4],
             size = size-1)  +
  # geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "ART", ],
  #            aes(Xnew_flipped,
  #                depth_below_ground_surface),
  #            colour = "white",
  #            size = size + 0.5) +
  # geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "ART", ],
  #            aes(Xnew_flipped,
  #                depth_below_ground_surface),
  #            colour = plasma(6)[5],
  #            size = size)  +
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "AF", ], # halo
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "white",
             size = size + 0.5) +
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "AF", ],
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = plasma(6)[6],
             size = size)  +

  scale_y_reverse(limits = c(3,0)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(colour = "black")) +
  scale_x_continuous(breaks = row_c,
                     labels = NULL) +
  xlab("") +
  ylab("Depth below \nground surface (m)") +
  scale_color_viridis(discrete=TRUE,
                      "Artefact\ntype") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  coord_equal()

row_c = c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
nums = paste0("B", 7:2)
row_mids <-  row_c[-length(row_c)] + diff(row_c)/2


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

# save copy
png("figures/CP-axes-and-grinding-stone_artefacts_SW_section.png",
    height = 1200,
    width = 1200*1.92,
    res = 300)
# antialias = "cleartype")
grid.draw(gt)
dev.off()


