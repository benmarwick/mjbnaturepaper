# paper with maps etc. http://faculty.washington.edu/bmarwick/PDFs/Clarkson_Jacobs_Marwick_2017.pdf

library(tidyverse)
library(mjbnaturepaper) # remotes::install_github("benmarwick/mjbnaturepaper")

# run code in the supplementary_information.Rmd up to around line 65

library(plotly)

mjb_geoarch_plotting <-
  cleaned_rotated_points_in_main_excavation_area %>%
  mutate(mm = str_detect(Description, "_MM"))

mjb_geoarch_plotting_mm_only <-
  mjb_geoarch_plotting %>%
  filter(mm)

# get phases for artefacts so we can see what phase the MM samples come from

# identify phases, we have phases() for rows 1-2-3 and front_phases() for rows 4-5-6
  back_phases_tbl <- phases()
  front_phases_tbl <- front_phases()

  mjb_geoarch_plotting_with_phases <-
  mjb_geoarch_plotting %>%
    mutate(exc_row = str_remove(Description_sq_sp, "_.{1,2}")) %>%
    mutate(exc_row = str_remove(exc_row, "[[:alpha:]]")) %>%
    mutate(exc_row = parse_number(exc_row)) %>%
    mutate(exc_loc = ifelse(exc_row %in% 1:3, "back", "front"))

  # non-equi join (join by range) to add phase information to depths

  mjb_geoarch_plotting_with_phases_front_phase <-
    mjb_geoarch_plotting_with_phases %>%
    filter(exc_loc == 'front') %>%
    fuzzy_left_join(
      front_phases_tbl,
      by = c("depth_below_ground_surface" = "upper",
             "depth_below_ground_surface" = "lower"),
      match_fun = list(`>=`, `<=`)) %>%
    distinct()

  mjb_geoarch_plotting_with_phases_back_phase <-
    mjb_geoarch_plotting_with_phases %>%
    filter(exc_loc == 'back') %>%
    fuzzy_left_join(
      back_phases_tbl,
      by = c("depth_below_ground_surface" = "upper",
             "depth_below_ground_surface" = "lower"),
      match_fun = list(`>=`, `<=`)) %>%
    distinct()

  # combine again into one table
  mjb_geoarch_plotting_with_phases <-
    bind_rows(mjb_geoarch_plotting_with_phases_front_phase,
              mjb_geoarch_plotting_with_phases_back_phase) %>%
    filter(!is.na(phase)) %>%
    # make nice informative point labels
    mutate(plot_label = paste0(Description,
                               ", phase: ",
                               phase,
                               ", row:",
                               exc_row,
                               ", ",
                               exc_loc))

# plot MM against all other points to see what phase the MM samples are in
  p <-
  plot_ly(data = mjb_geoarch_plotting_with_phases,
          x = ~Xnew_flipped,
          y = ~Ynew,
          z = ~Elevation) %>%
    add_markers(
      size = 0.1,
      alpha = 0.9,
      alpha_stroke = 0.9,
      color = ~as.factor(phase),
      text = ~plot_label,
      mode = "markers") %>%
    add_markers(data = mjb_geoarch_plotting_mm_only,
                x = ~Xnew_flipped,
                y = ~Ynew,
                z = ~Elevation,
                text = ~Description,
                mode = "markers")

htmlwidgets::saveWidget(p, "figures/mm-sample-locations-with-phases.html")

# now we need to study this plot to identify the
# - phase (age)
# - row (front-back)
# of each MM sample

# look up in the table to see what phases the MM samples were assigned to:
  mjb_geoarch_plotting_with_phases_inspect_df <-
  mjb_geoarch_plotting_with_phases %>%
    filter(mm) %>%
    select(Description,
           exc_row,
           exc_loc,
           phase)


