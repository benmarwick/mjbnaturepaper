
# run all code in the SI Rmd first


# only plot one point per artefact (some artefacts have multiple total station points)
stone_artefacts_only_one <-
  stone_artefacts_only %>%
  group_by(Description, find) %>%
  dplyr::summarise(Xnew_flipped = mean(Xnew_flipped),
                   depth_below_ground_surface = mean(depth_below_ground_surface))



# determined by plotting row C end levels
row_c <- c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
row_mids <- row_c/2
size = 0.5

library(viridis)
library(grid)
library(shadowtext)

p <-
  ggplot() +
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "L", ],
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "grey80",
             size = size) +
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "AXE", ],
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "grey80",
             size = size)  +
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "ART", ],
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "grey80",
             size = size) +
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "ART", ],
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "grey80",
             size = size)  +
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "AF", ], # halo
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "grey80",
             size = size) +
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "AF", ],
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "grey80",
             size = size)  +

  # show haematite pieces on top
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "HM", ],
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "red",
             size = size+1)  +

  # show grinding pieces on top
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "GS", ],
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "blue",
             size = size+0.5)  +

  geom_shadowtext(data =  osl_ages,
             aes(Xnew_flipped,
                 depth_below_ground_surface,
                 label = paste0(osl_age, " ka ")),
            size = size + 2,
            colour = "black",
            bg.color = "white",
            bg.r = 0.1
             ) +

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


# add square labels
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
png("figures/stone_artefacts_SW_section.png",
    height = 1200,
    width = 1200*1.92,
    res = 300)
# antialias = "cleartype")
grid.draw(gt)
dev.off()
