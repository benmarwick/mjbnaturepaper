
# run all code in the SI Rmd first


# only plot one point per artefact (some artefacts have multiple total station points)
stone_artefacts_only_one <-
  stone_artefacts_only %>%
  group_by(Description, find) %>%
  dplyr::summarise(Xnew_flipped = mean(Xnew_flipped),
                   depth_below_ground_surface = mean(depth_below_ground_surface))


# BLM wants paired OSL and C14 ages, like we have in fig 1 of
# https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-020-14723-0/MediaObjects/41467_2020_14723_MOESM1_ESM.pdf
# prepare data
excluding <- c("Wk43605" , # pit feature
               #"Wk43606",
               #"Wk43607",
               #"Wk43610",
               #"Wk43604",
               #"Wk43611",
               #"OZT591",
               "OZT593",
               "OZR149"
)

# excluding <- NA

squares <- "B3|B4|B5|C3|C4|C5|D3|D4|D5|E3|E4|E5"

c14_ages_excludes <-  c14_ages[!grepl(paste0(excluding, collapse = "|"), c14_ages$Lab.ID), ]
c14_ages_excludes <- c14_ages_excludes[grepl(paste0(squares, collapse = "|"), c14_ages_excludes$square), ]
c14_ages_excludes$Bchron_Median_ka <- round(c14_ages_excludes$Bchron_Median /1000, 1)

# determined by plotting row C end levels
row_c <- c(2.4, 1.4, 0.4, -0.6, -1.6, -2.6, -3.6)
row_mids <- row_c/2
size = 0.5

library(viridis)
library(grid)
library(shadowtext)
library(ggrepel)

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

  # colours from https://coolors.co/264653-2a9d8f-e9c46a-f4a261-e76f51

  # show haematite pieces on top
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "HM", ],
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "#e76f51",
             size = size)  +

  # show grinding pieces on top
  geom_point(data = stone_artefacts_only_one[stone_artefacts_only_one$find == "GS", ],
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             colour = "#264653",
             size = size+0.5,
             shape = 15)  +

  # show OSL ages
  geom_text_repel(data =  osl_ages,
             aes(Xnew_flipped,
                 depth_below_ground_surface,
                 label = paste0(osl_age, " ka ")),
            size = size + 2.5,
            colour = "black",
            bg.color = "white",
            bg.r = 0.1,
            force = 30,
            min.segment.length = 0.3,
            max.time = 5
             ) +
  # white halo on diamonds
  geom_point(data =  osl_ages,
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             size = size + 3.5 + 0.5,
             colour = "white",
             shape = 18 # diamond
  ) +
  geom_point(data =  osl_ages,
                  aes(Xnew_flipped,
                      depth_below_ground_surface),
                  size = size + 2.5,
             colour = "#2a9d8f",
             shape = 18 # diamond
  ) +
  # show C14 ages
  geom_text_repel(data =  c14_ages_excludes,
                  aes(Xnew_flipped,
                      depth_below_ground_surface,
                      label = paste0(Bchron_Median_ka, " cal kBP ")),
                  size = size + 2.5,
                  colour = "black",
                  bg.color = "white",
                  bg.r = 0.1,
                  force = 30,
                  min.segment.length = 0.3,
                  max.time = 5
  ) +
  # white halo on triangles
  geom_point(data =  c14_ages_excludes,
             aes(Xnew_flipped,
                 depth_below_ground_surface),
             size = size + 2.5 + 0.5,
             colour = "white",
             shape = 17 # triangle
  ) +

  geom_point(data =  c14_ages_excludes,
                  aes(Xnew_flipped,
                      depth_below_ground_surface),
                  size = size + 2.5,
             colour = "#f4a261",
             shape = 17 # triangle
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
png("figures/stone_artefacts_ages_SW_section_for_BLM.png",
    height = 1200,
    width = 1200*1.92,
    res = 300
    )
# antialias = "cleartype")
grid.draw(gt)
dev.off()
dev.off()

# save copy
svg("figures/stone_artefacts_ages_SW_section_for_BLM.svg",
    height = 1200,
    width = 1200*1.92
)
# antialias = "cleartype")
grid.draw(gt)
dev.off()
dev.off()



# GH says some OSL ages are different
# I compared stuff in R to published SI and see these different
# me vs SI
# SW7C 91.2 vs 79
# SW4C 59.2 vs 62.7
# SW3C 60 vs 64.0
# NW14 67.8 vs 62.8
# SW13A 64.5 vs 62.6
# SW2C 68.8 vs 64.9
# NW13 60.3 vs 58.2
# NW12 56.9 vs 55.5
# SW11A 63.8 vs 63.4
# NW11 54.2 vs 52.6
# SW10A  55.3 vs 55.3
# NW15 50.3 vs 48.4
# NW10  46.4 vs 43.8
# SW8A 40.8 vs 42.8
# ... etc looks like ZJ sent me the not-final version of the published ages












