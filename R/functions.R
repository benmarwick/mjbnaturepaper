#' mjbnaturepaper: A package containing code and data for analysis of the 2012 and 2015 MJB excavations
#'
#' The foo package provides ...
#'
#' @section Foo functions:
#' The foo functions ...
#'
#' @docType package
#' @name mjbnaturepaper
NULL

#' read_2015_total_station_data
#'
#' @param total_station_csv_directory_2015 A directory containing one or more CSV files
#'
#' @return a data frame
#' @importFrom stats na.omit
#' @importFrom utils read.csv
#' @export
#'
read_2015_total_station_data <- function(total_station_csv_directory_2015){

  # read in CSV for 2015 data: "data/MKII15_04JUL15.CSV"

  files_2015 <- dir(total_station_csv_directory_2015,
               recursive=TRUE,
               full.names=TRUE,
               pattern="\\.(csv|CSV)$")
  # read in the CSV files and add the filename of each file as a column to
  # each dataset so we can trace back dodgy data
  # create a function to read tehe CSV and get filenames
  read.tables <- function(file.names, ...) {
    plyr::ldply(file.names, function(fn) data.frame(Filename=fn, read.csv(fn, ...)),.progress = 'text')
  }
  # execute function
  ts_data <- read.tables(files_2015, stringsAsFactors = FALSE)

  # simplify to join with 2012 data
  ts_data_2015 <-  ts_data[,c('Description', 'Easting', 'Northing', 'Elevation')]
  ts_data_2015$year <- "2015"

  # remove duplicates
  # ts_data_2015 <- ts_data_2015[!duplicated(ts_data_2015),]


  return(ts_data_2015)
}

#' read_2012_total_station_data
#'
#' @param total_station_csv_directory_2012 A directory containing one or more CSV files
#'
#' @return a data frame
#'
#' @importFrom plyr ldply
#' @importFrom stats na.omit
#' @importFrom utils read.csv
#' @export
#'
read_2012_total_station_data <- function(total_station_csv_directory_2012){

  # "data/MII 2012 Total_Station_Data"
  files_2012 <- dir(total_station_csv_directory_2012,
               recursive=TRUE,
               full.names=TRUE,
               pattern="\\.(csv|CSV)$")
  # read in the CSV files and add the filename of each file as a column to
  # each dataset so we can trace back dodgy data
  # create a function to read tehe CSV and get filenames
  read.tables <- function(file.names, ...) {
    plyr::ldply(file.names, function(fn) data.frame(Filename=fn, read.csv(fn, ...)),.progress = 'text')
  }
  # execute function
  points.all <- read.tables(files_2012, stringsAsFactors = FALSE)

  # put all the upper case
  points.all$description <- toupper(points.all$description)
  points.all$Description <- toupper(points.all$Description)

  # how many NAs in the Desc?
  # nrow(points.all)
  # length(points.all$description[points.all$description == "NA"])
  # length(points.all$Description[points.all$Description == "NA"])

  # if 'Description' is empty, move the contents of the 'description' into it

  points.all$Description <-
    ifelse(is.na(points.all$Description),
           points.all$description,
           points.all$Description)


  # simplify x-y-z columns for days when different headers were used by the total station operator
  points.all$Xx <-
    ifelse(grepl('[0-9]+', points.all$x),
           points.all$x,
       ifelse(grepl('[0-9]+', points.all$Easting),
              points.all$Easting,
          ifelse(grepl('[0-9]+', points.all$Eastig),
                 points.all$Eastig,
                 points.all$X)))


points.all$Yy <-
  ifelse(grepl('[0-9]+', points.all$y),
         points.all$y,
      ifelse(grepl('[0-9]+', points.all$Northing),
             points.all$Northing, points.all$Y))


points.all$Zz <-
  ifelse(grepl('[0-9]+', points.all$z),
         points.all$z,
    ifelse(grepl('[0-9]+', points.all$Z),
           points.all$Z, points.all$Elevation))


  # remove duplicates by looking at the location and removing points
  # that have the exact same x-y-z coord
  points.all$coords <-  paste0(points.all$Xx, points.all$Yy, points.all$Zz)

  # identify and remove duplicate rows in the data frame
  points.all <- points.all[!duplicated(points.all$coords), ]

  # On some days it seems like the total station swapped columns
  # for northing and easting in the CSV files, so we check to make
  # sure Northing is always greater than Easting. They were originally
  # set at 5000, 5000 (better would have been 5000, 2000, so
  # it's obvious which is which). Y must always be bigger than X
  # for correct measurements

  points.all$xgreaterthany <- ifelse(points.all$Xx > points.all$Yy, "X>Y", "ok")

  points.all$Xall <- ifelse(points.all$xgreaterthany == "X>Y", points.all$Yy, points.all$Xx)
  points.all$Yall <- ifelse(points.all$xgreaterthany == "X>Y", points.all$Xx, points.all$Yy)


  surf <- 100.693213   # NE_SEC_TAPE_1
  points.all$depth.below.surface <-  surf - points.all$Zz


  # now that we've got the right angle, rotate all the artefact data...
  # limit to dig area
  EL <- points.all[grepl("EL_", points.all$Description),]


  points.all <- points.all[points.all$Xall < max(na.omit(EL$Xall)) &
                      points.all$Xall > min(na.omit(EL$Xall)) &
                      points.all$Yall < max(na.omit(EL$Yall)) &
                      points.all$Yall > min(na.omit(EL$Yall))
                    , ]

  # make some common column names
  points.all$Easting <- points.all$Xall
  points.all$Northing <- points.all$Yall
  points.all$Elevation <- points.all$Zz
  # already have 'Description'

  # simplify to join with 2015 data
  ts_data_2012 <-  points.all[,c('Description', 'Easting', 'Northing', 'Elevation')]
  ts_data_2012$year <- "2012"

return(ts_data_2012)

}


#' combine_2012_and_2015_total_station_data
#'
#' @param total_station_data_2012
#' @param total_station_data_2015
#'
#' @return a data frame
#' @export
#'
#' @importFrom dplyr %>% filter
#'
combine_2012_and_2015_total_station_data <- function(total_station_data_2012,
                                                     total_station_data_2015){
  # combine 2012 and 2015 data --------------------------------------------------

  ts_data_both_years <- rbind(total_station_data_2012,
                              total_station_data_2015)

}


#' extract_points_in_main_excavation_area
#'
#' @param combined_2012_and_2015_total_station_data
#'
#' @return a data frame
#' @export
#'
#' @importFrom dplyr %>% filter
extract_points_in_main_excavation_area <- function(combined_2012_and_2015_total_station_data){

  # still a few stray points way out of the excavation area, so remove those with Easting < 4995, Northing >5015
  combined_2012_and_2015_total_station_data_inside <-
    combined_2012_and_2015_total_station_data %>%
    filter(Easting > 4995, Northing < 5015)

# get only labelled finds and other points of interest
  excavation_area <- c("PF", "NAIL", "OSL", "EL", "P", "PL", "GAMMA", "MM", "SL")

  ts_data_both_years_PFs <- combined_2012_and_2015_total_station_data_inside %>%
    filter(grepl(paste(excavation_area, collapse = "|"), Description) )

  return(ts_data_both_years_PFs)

}


#' rotate_points_in_main_excavation_area
#'
#' @param extracted_points_in_main_excavation_area
#'
#' @return a data frame
#' @importFrom stats na.omit
#' @importFrom dplyr mutate
#' @export
#'
rotate_points_in_main_excavation_area <- function(extracted_points_in_main_excavation_area){

  # Rotate points from both seasons -----------------------------------------


  # get means

  lx <- mean(na.omit(extracted_points_in_main_excavation_area$Easting))
  ly <- mean(na.omit(extracted_points_in_main_excavation_area$Northing))

  # first translate all points to new origin

  extracted_points_in_main_excavation_area$X <-
    extracted_points_in_main_excavation_area$Easting - lx
  extracted_points_in_main_excavation_area$Y <-
    extracted_points_in_main_excavation_area$Northing - ly

  # check with a quick look, should
  # have 0,0 in the middle of the plot

  # rotate around new origin
  deg = 46 #
  theta = deg*pi/180
  extracted_points_in_main_excavation_area$Xnew <-
    (extracted_points_in_main_excavation_area$X * cos(theta)) - (extracted_points_in_main_excavation_area$Y * sin(theta))

  extracted_points_in_main_excavation_area$Ynew <-
    (extracted_points_in_main_excavation_area$Y * cos(theta)) + (extracted_points_in_main_excavation_area$X * sin(theta))


  # I want to flip it on the vertical axis --------------

  # change the sign on the Xnew variable
  extracted_points_in_main_excavation_area <-
    extracted_points_in_main_excavation_area %>%
    dplyr::mutate(Xnew_flipped = -Xnew)

  return(extracted_points_in_main_excavation_area)

}


#' clean_points_in_main_excavation_area
#'
#' @param rotated_points_in_main_excavation_area
#'
#' @return a data frame
#' @importFrom dplyr %>% filter
#' @import stringr
#' @import purrr
#' @import tidyr
#' @export
#'
#' @examples
clean_points_in_main_excavation_area <- function(rotated_points_in_main_excavation_area){

  # Clean data to deal with non-artefact and mis-classifications-------------

  ts_data_both_years_PFs <- rotated_points_in_main_excavation_area

  # remove the list column
  ts_data_both_years_PFs$code <- NULL

  # remove some non-artefacts from CC's email

  # haematite to remove
  remove_haematite <- c("HM654", "HM1391")

  # lithics to remove
  col1 <- c(4488, 6868, 9285, 7378, 7379, 9104, 7615, 7745, 7553, 2599, 2723, 2532, 3103, 3244, 2623)

  col2 <- c(8263, 9001, 7817, 3086, 1775, 1813, 1841, 1852, 1853, 6138, 3827, 3873, 4318, 3864, 7075)

  col3 <- c(7524, 8701, 8446, 9190, 7164, 8547, 8857, 8533, 3081, 1848, 1815, 6976, 6794, 9031, 6084, 5992, 6712, 8548, 8494, 8560, 8503, 8995)

  col4 <- c(8704, 8462, 8868, 8475, 8977, 9017, 9047, 8721, 8065, 9200, 8605, 9026, 3083, 9023, 1830, 6098, 8574, 9143, 8666, 8669, 8626, 8703, 8719, 8789, 9007, 9020, 9045)

  col5 <- c(7957, 7928, 8712, 8058, 8707, 8592, 8972, 7841, 8558, 8356, 7906, 7944, 8596, 8372, 8587, 8668, 8609, 8625, 8595, 8579, 8752, 8705, 8729, 9085, 9046)

  col6 <- c(7724, 7347, 7538, 9009, 7550, 7528, 7783, 7846, 7938, 7979, 8033, 8197, 8229, 8396, 8401, 8488, 8501, 8502, 8611, 8612, 8638, 8658, 9075, 9151, 9199)

  email <- c(75, 131, 185, 1409, 1725, 1666)

  # numbers to remove
  remove <- c(col1, col2, col3, col4, col5, col6, email)


  # delete the very lowest putting HM and ART

  lowest_haematite <-  ts_data_both_years_PFs %>%
    filter(grepl("HM", Description)) %>%
    filter(Elevation == min(Elevation))
  # remove it
  ts_data_both_years_PFs <- ts_data_both_years_PFs %>%
    filter(!grepl(lowest_haematite$Description, Description))

  lowest_art <-  ts_data_both_years_PFs %>%
    filter(grepl("ART", Description)) %>%
    filter(Elevation == min(Elevation))

  # remove it (and all the points associated with it)
  ts_data_both_years_PFs <- ts_data_both_years_PFs %>%
    filter(!grepl(lowest_art$Description, Description))


  # add L to them
  remove_lithics <- paste0("L", remove)

  remove_stuff <- c(remove_lithics, remove_haematite)

  # do the actual removal
  ts_data_both_years_PFs <- ts_data_both_years_PFs %>%
    filter(!grepl(paste(remove_stuff, collapse = "|"), Description))

  # L9060 and R299 are grindstones, so re-write description

  ts_data_both_years_PFs$Description <-  with(ts_data_both_years_PFs, gsub("L9060", "GS9060X", Description))

  ts_data_both_years_PFs$Description <-  with(ts_data_both_years_PFs, gsub("R299", "GS299X", Description))

  # check
  # grep("GS299X", ts_data_both_years_PFs$Description)


  # Split description column out into separate cols --------------


  ts_data_both_years_PFs$Description <- as.character(ts_data_both_years_PFs$Description)
  ts_data_both_years_PFs$code <- (strsplit(ts_data_both_years_PFs$Description, "_"))
  ts_data_both_years_PFs$type <- sapply(ts_data_both_years_PFs$code, "[", 1)
  ts_data_both_years_PFs$square <- gsub("^\\s+|\\s+$", "", sapply(ts_data_both_years_PFs$code, "[", 2))
  ts_data_both_years_PFs$spit <- sapply(ts_data_both_years_PFs$code, "[", 3)
  ts_data_both_years_PFs$cnr <- ifelse(nchar(sapply(ts_data_both_years_PFs$code, "[", 4)) == 1,  sapply(ts_data_both_years_PFs$code, "[", 4), "")
  ts_data_both_years_PFs$find <- ifelse(grepl("P", ts_data_both_years_PFs$type), str_extract(sapply(ts_data_both_years_PFs$code, "[", 4), "[A-Z]+"),"")
  ts_data_both_years_PFs$findn <- ifelse(grepl("P", ts_data_both_years_PFs$type), str_extract(sapply(ts_data_both_years_PFs$code, "[", 4), "[0-9]+"),"")

  # go over each character column and remove leading spaces
  tmp <-  ts_data_both_years_PFs
  ts_data_both_years_PFs <- dmap_if(ts_data_both_years_PFs,
                                    is.character,
                                    function(i) gsub("^ ", "", i))

  # check if we removed spaces
  # type_col_before <- tmp$type
  # type_col_before[grep("^ ", type_col_before)] # lots of spaces
  #
  # type_col_after <- ts_data_both_years_PFs$type
  # type_col_after[grep("^ ", type_col_after)] # none with spaces
  #


  # # if the row is an EL, and square column has 4 characters,
  # split them and put the last 2 in the spit col. Esp for C3 and C4 spit 14
  tmp_ <-
    ts_data_both_years_PFs %>%
    filter(grepl("EL", Description),
           nchar(square) == 4) %>%
    mutate(cnr = spit) %>%
    separate(square, c("square", "spit_"), sep = 2) %>%
    mutate(spit_ = as.numeric(gsub("[^0-9]", "", spit_))) %>%
    mutate(spit = spit_) %>%
    mutate(spit_ = NULL)

  # replace in main data frame
  ts_data_both_years_PFs$square <-
    ifelse(ts_data_both_years_PFs$Description %in% tmp_$Description,
           tmp_$square,
           ts_data_both_years_PFs$square)

  ts_data_both_years_PFs$spit <-
    ifelse(ts_data_both_years_PFs$Description %in%  tmp_$Description,
           tmp_$spit,
           ts_data_both_years_PFs$spit)

  # from CC email 22 APril 2016
  # OSL sample labels are wrong for the SW_A column.
  # They should all be one lower -
  # so 12A should be 11A, 11A should be 10A  etc.
  check_osl <- ts_data_both_years_PFs %>%
    filter(type == "OSL",
           square == "SW",
           grepl("A", spit)) %>%
    separate(spit, c("spit_number, spit_letter"),  sep = -2, remove = FALSE)

  # same
  check_osl <- ts_data_both_years_PFs %>%
    filter (grepl("OSL", Description) &
              grepl("SW", Description)  &
              grepl("A", Description)) %>%
    separate(spit, c("spit_number, spit_letter"),  sep = -2, remove = FALSE)

  # function that takes col, splits alpha-num, subtracts one from the num, and pastes them back together
  options(warn=-1)
  my_fn <- function(x){
    split_ <- separate(x, spit, c("spit_number", "spit_letter"),  sep = -2, remove = FALSE)
    # subtract one from the OSL number
    split_$spit_number <-  as.numeric(split_$spit_number) - 1
    # remake OSL sample ID with -1
    split_$spit <- NULL
    split_ <- unite(split_, spit, spit_number, spit_letter, sep ="")
    return(split_$spit)
  }
  options(warn=0)

  # select the cols and rows to change

  # we also need to update the description.
  # looking at ZJ's word doc of tables
  # ZJ has SW2A-11, 13A and 14A: no twelve!
  # I have SW3A-13A: no 14!
  # our 1-12 are one off

  conditions <- (with(ts_data_both_years_PFs, # conditions
                      grepl("OSL", Description) &
                        grepl("SW", Description)  &
                        grepl("A", Description) &
                        grepl(paste0("_", 1:12, "A", collapse = "|"), Description) ))

  # check
  # ts_data_both_years_PFs[ conditions, ]
  # check
  # sum(conditions)

  # first update desc

  ts_data_both_years_PFs$Description <-
    ifelse(conditions,
           paste0("OSL_SW_",
                  my_fn(ts_data_both_years_PFs)),
           ts_data_both_years_PFs$Description)

  # second update spit.
  # the ifelse function that selects the desired rows of the desired column, and changes them
  ts_data_both_years_PFs$spit <- ifelse(conditions, my_fn(ts_data_both_years_PFs), ts_data_both_years_PFs$spit)

  # remove duplicates
  # length(ts_data_both_years_PFs[duplicated(ts_data_both_years_PFs),])

  # check again

  ts_data_both_years_PFs$dup_string <- with(ts_data_both_years_PFs,
                                  paste0(Description,
                                         Easting,
                                         Northing,
                                         Elevation))
  #
  # length(ts_data_2012$dup_string)
  # length(unique(ts_data_2012$dup_string))

  ts_data_both_years_PFs <- ts_data_both_years_PFs[!duplicated(ts_data_both_years_PFs$dup_string),]

  ts_data_both_years_PFs$dup_string <- NULL



  return(ts_data_both_years_PFs)


}


#' stone_artefacts
#'
#' @param cleaned_rotated_points_in_main_excavation_area
#'
#' @return A data frame
#' @export
#'
#' @examples
stone_artefacts <- function(cleaned_rotated_points_in_main_excavation_area){

  the_points <- cleaned_rotated_points_in_main_excavation_area

  # get only points for lithics, axes, axes flakes, haematite, grinding and art

  want <- c("_L", "_HM", "_GS", "_AX", "_AF", "_AR")

  the_points <- the_points[grepl(paste0(want, collapse = "|"),
                                 the_points$Description), ]

  return(the_points)

}


#' refits
#'
#' @param stone_artefacts
#'
#' @return A data frame
#' @export
#'
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import ggrepel
#'
refits <- function(stone_artefacts){

  # read in data
  refit_data <- read.csv("data/refit_data/Table_6_McNeil_Jessica_41449086_BA(Hons)_finalthesis.csv", stringsAsFactors = FALSE, header = TRUE)
  refit_data <- refit_data[-1,]


  appendix_data <- read.csv("data/refit_data/Appendix McNeil_Jessica_41449086_BA(Hons)_finalthesis.csv", stringsAsFactors = FALSE)

  # get coords of refits with L numbers

  # wide to long
  names(refit_data)[2:3] <- c("set", "refit")

  refit_data_long <-
    gather(refit_data[,-1],
           key = artefact,
           value = descr,
           -set, -refit)

  refit_data_long$artefact_Lnums <-  gsub(" ", "", str_extract(refit_data_long$descr,
                                             "\\bL.* \\b"))

  refit_data_long$artefact_Lnums[ refit_data_long$artefact_Lnums == "L3024.2"] <- "L3024"

  L_nums_artefact <- gsub(" ", "",
                            na.omit(refit_data_long$artefact_Lnums))

  L_nums_artefact[L_nums_artefact == "L3024.2"] <- "L3024"

  artefact_L_details <- stone_artefacts[grepl(paste0(L_nums_artefact,
                                                     collapse = "|"),
                                              stone_artefacts$Description),]

  # Jess has D2_41 for L3024, so exclude the duplicates
  # and C4_32 for L1718
  artefact_L_details <- artefact_L_details[!grepl(
"PF_C2_41_L3024|PF_C5_18_L3024|PF_E2_32A_L1718",
                                                  artefact_L_details$Description),]
  # for joining
  artefact_L_details$artefact_Lnums <- str_extract(artefact_L_details$Description,
                                                   "L[0-9]*")



  # for artefacts with no L number, get midpoint of spit end levels

  refit_data_long$artefact_sq_sp <-     gsub("-", "_",
                                        gsub(" ", "",
                                        str_extract(refit_data_long$descr,
                                              "[A-Z]{1}[0-9]{1}-[0-9]{1}.*")))


  # average location for all lithics in those squares and spits

  stone_artefacts_in_sqs <-
  stone_artefacts %>%
    filter(grepl(paste0(refit_data_long$artefact_sq_sp,
                        collapse = "|"), Description)) %>%
    dplyr::mutate(artefact_sq_sp = paste0(square, "_", spit)) %>%
    dplyr::group_by(artefact_sq_sp) %>%
    dplyr::summarise(Easting = mean(Easting),
                     Northing = mean(Northing),
                     Elevation = mean(Elevation),
                     Xnew_flipped = mean(Xnew_flipped),
                     Ynew = mean(Ynew)) %>%
    arrange(Elevation)

  # put the coords onto the refit table, split, apply combine!

  # split table
  refit_data_long_Lnums <- refit_data_long[!is.na(refit_data_long$artefact_Lnums),]
  refit_data_long_no_Lnums <- refit_data_long[is.na(refit_data_long$artefact_Lnums),]

  # join coords on
  refit_data_long_Lnums <- left_join(refit_data_long_Lnums,
                                     artefact_L_details,
                                     by = "artefact_Lnums")

  refit_data_long_no_Lnums <- left_join(refit_data_long_no_Lnums,
                                        stone_artefacts_in_sqs,
                                     by = "artefact_sq_sp")

  # get the number of cols to be the same
  refit_data_long_Lnums <-
      refit_data_long_Lnums[,
                            colnames(refit_data_long_Lnums) %in%
                              names(refit_data_long_no_Lnums)]

  names(refit_data_long_Lnums)
  names(refit_data_long_no_Lnums)

  # combine into one table
  refit_data_long_coords <- rbind(refit_data_long_Lnums,
                                  refit_data_long_no_Lnums)

  refit_data_long_coords$descr <- gsub("^ | $", "", refit_data_long_coords$descr)

  # spit into start and end points to plot
  refit_data_long_coords_start <-
    refit_data_long_coords[duplicated(refit_data_long_coords$set), ]

  refit_data_long_coords_end <-
    refit_data_long_coords[!refit_data_long_coords$descr %in% refit_data_long_coords_start$descr, ]

  # long to wide for plotting lines connecting refits

  refit_data_coords_wide <-
  left_join(refit_data_long_coords_start,
            refit_data_long_coords_end,
            by = "set")

  # plot
  ggplot() +
    geom_point(data = refit_data_long_coords,
                    aes(Xnew_flipped,
                        Elevation),
                    colour = "red") +

    geom_text_repel(data = refit_data_long_coords,
                    aes(Xnew_flipped,
                  Elevation,
                  label = descr)) +

    geom_segment(data = refit_data_coords_wide,
                 aes(x = Xnew_flipped.x,
                     y = Elevation.x,
                     xend = Xnew_flipped.y,
                     yend = Elevation.y),
                 size = 1,
                 colour = "green") +

    coord_equal() +
    theme_minimal()

  ggsave("figures/refit_elev.png", width = 15)

  # plot
  ggplot() +
    geom_point(data = refit_data_long_coords,
               aes(Xnew_flipped,
                   Ynew),
               colour = "red") +

    geom_text_repel(data = refit_data_long_coords,
                    aes(Xnew_flipped,
                        Ynew,
                        label = descr)) +

    geom_segment(data = refit_data_coords_wide,
                 aes(x = Xnew_flipped.x,
                     y = Ynew.x,
                     xend = Xnew_flipped.y,
                     yend = Ynew.y),
                 size = 1,
                 colour = "green") +

    coord_equal() +
    theme_minimal()

  ggsave("figures/refit_plan.png", width = 15)

  # what is the distance between the two refit pieces?
  refit_data_coords_wide$refit_dists_m <-
    sqrt((refit_data_coords_wide$Easting.x -
            refit_data_coords_wide$Easting.y)^2 +
           (refit_data_coords_wide$Northing.x -
              refit_data_coords_wide$Northing.y)^2 +
           (refit_data_coords_wide$Elevation.x -
              refit_data_coords_wide$Elevation.y)^2)

  #  plot
  ggplot(refit_data_coords_wide,
         aes(refit_dists_m)) +
    geom_histogram() +
    theme_minimal()

  ggsave("figures/refit_dists_histogram.png", width = 15)

  # what is the plunge angle of the refits?
  # compute from elevations and X (Easting.x or Xnewflipped)
  # plot the angles
  # start locations
  start_coords <- data.frame(E = refit_data_coords_wide$Easting.x,
                             N = refit_data_coords_wide$Northing.x,
                             Z = refit_data_coords_wide$Elevation.x)

  # end locations
  end_coords <-   data.frame(E = refit_data_coords_wide$Easting.y,
                             N = refit_data_coords_wide$Northing.y,
                             Z = refit_data_coords_wide$Elevation.y)

  # set end coords so that start is 0,0
  # assume that the higher point is the origin
  zero_origin <- data.frame(matrix(nrow = nrow(end_coords), ncol = ncol(end_coords)))
    for(i in 1:nrow(zero_origin)){

      if(start_coords$Z[i] < end_coords$Z[i]){
        zero_origin[i,] <-  end_coords[i,] - start_coords[i,]
      } else {
        zero_origin[i,] <- start_coords[i,] - end_coords[i,]
      }
    }


  # compute angle of displacement, as 0-360 deg
  refit_angle <- atan2(zero_origin[,1], zero_origin[,3]) / (pi/180)
  refit_angle_full_circle <- ifelse(refit_angle < 0, 360 + refit_angle, refit_angle)
  refit_angle_abs <- abs(refit_angle)
  # axial to bidir via the 'Angle Doubling Procedure'
  refit_angle_bidirectional <- c(refit_angle_abs,
                                 ifelse(refit_angle_abs * 2 > 360,
                                        (refit_angle_abs * 2) - 360,
                                        refit_angle_abs * 2))

  # sig test for orientations, these data are axial
  # Axial data are converted to angular data by multiplying by two.
  refit_angle_rads <- abs(refit_angle_bidirectional) * pi/180
  refit_angle_rads_test <- circular::rayleigh.test(refit_angle_rads)


  g1 <- ggplot() +
    geom_histogram(aes(x = refit_angle_abs),
                   colour = "black",
                   fill = "grey80") +
    theme(axis.text.x = element_text(size = 18)) +
    coord_polar(start = 0) +
    scale_x_continuous(limits = c(0, 360), breaks = c(0, 90, 180, 270)) +
    theme_minimal(base_size = 14) +
    xlab("") +
    ggtitle(paste0("MJB refit orientations \n (Rayleigh = ",
                   round(refit_angle_rads_test$statistic, 3),
                   " p = ",
                   round(refit_angle_rads_test$p.value, 3),
                   ")"))
  g1
  # grid::grid.newpage()
  # grid::pushViewport(grid::viewport(height=1, width=1, clip="on"))
  # grid::grid.draw(g1)
  # grid::grid.rect(x=0,y=0,height=2, width=1.05, gp=grid::gpar(col="white"))

  ggsave("figures/refits orientations.png")

  ## correlation between size of arefact and distance of refit?



}



#' geoarchaeology
#'
#' @param cleaned_rotated_points_in_main_excavation_area
#'
#' @return A data frame
#' @export
#'
#' @import Hmisc
#' @import dplyr
#' @import analogue
#' @import readxl
#'
#' @examples
geoarchaeology <- function(cleaned_rotated_points_in_main_excavation_area){


  # mag sus from KL
  mag_sus_B2 <- read.csv("data/geoarchaeology_data/Kelsey_Lowe_mag_sus_B2.csv")
  # remove the C3 stuff
  mag_sus_B2$Square <- NULL
  mag_sus_B2$Depth <- NULL
  mag_sus_B2$Spit <- NULL

  mag_sus_B2$Depth.Below.Surface..m <-
    as.numeric(mag_sus_B2$Depth.Below.Surface..cm.) / 100

  # depths were measured on a tape, we shot some points on that tape. Let's find them
  # SW wall tape... actually NE_SEC_TAPE 1-4 (Not SECT)
  # are points on the tape on the SW wall,
  # as we can see from the 3d plot of section things

  # NE_SECT_TAPE 1 is the ground surface, then the other points are one meter apart


  NE_SEC_TAPE <-
    cleaned_rotated_points_in_main_excavation_area[grepl("NE_SEC_TAPE",
                      cleaned_rotated_points_in_main_excavation_area$Description), ]

  # put tape and total station depths together
  tape_depths <- data.frame(tape = 0:3,
                            total_station = NE_SEC_TAPE$Elevation)

  # need to interpolate to 5 cm to match with mag_sus_B2$Depth.Below.Surface..cm.

  xout <- seq(min(mag_sus_B2$Depth.Below.Surface..m),
              max(mag_sus_B2$Depth.Below.Surface..m),
              by = 0.05)

  # approx returns a list, convert to dataframe
  # we also need to extrapolate a little
  tape_depths_interp <- data.frame(do.call(cbind,
                                           approxExtrap(tape_depths$tape,
                                                        tape_depths$total_station,
                                                        n = length(xout),
                                                        xout = xout)))
  names(tape_depths_interp) <- names(tape_depths)


  # attach interpolated depths to mag sus data
  # there is a tiny difference in the vectors, so we convert to chr

  mag_sus_B2$Depth.Below.Surface..m  <-
    as.character(mag_sus_B2$Depth.Below.Surface..m)

  tape_depths_interp$tape <-  as.character(tape_depths_interp$tape)

  mag_sus_B2_total_station <-
    dplyr::left_join(mag_sus_B2,
              tape_depths_interp,
              by = c('Depth.Below.Surface..m' = 'tape'))

  mag_sus_B2_total_station$Depth.Below.Surface..m <-
    as.numeric(mag_sus_B2_total_station$Depth.Below.Surface..m)

  # depth below surf from total station
  surf <- 100.693213   # NE_SEC_TAPE_1
  mag_sus_B2_total_station$total_station_depth_m <-
    surf - mag_sus_B2_total_station$total_station

  # error in tape measure and total station
  mag_sus_B2_total_station$error <-
    mag_sus_B2_total_station$Depth.Below.Surface..m -
    mag_sus_B2_total_station$total_station_depth_m

  # done with mag sus

  # particle size data, sand-silt-clay


  # done with particle size data
  # from

  # dC13 data from Mara Page

  dC13_data <- read_excel("data/geoarchaeology_data/Page_160404_CombinedResults final full.xlsx")

  # clean_and_tidydC13_data

  # data are in rows 4 to 78
  dC13_data_rows <- c(4:78)
  # sample IDs, etc
  sample_ID <- dC13_data[ dC13_data_rows, ][[2]]
  sample_mass <- as.numeric(dC13_data[ dC13_data_rows, ][[9]])
  d15N <- as.numeric(dC13_data[ dC13_data_rows, ][[15]])
  d13C_accuracy <- as.numeric(dC13_data[ dC13_data_rows, ][[23]])
  d13C_precision <- as.numeric(dC13_data[ dC13_data_rows, ][[24]])
  amount_corrected <- as.numeric(dC13_data[ dC13_data_rows, ][[25]])
  percent_C <- as.numeric(dC13_data[ dC13_data_rows, ][[19]])

  #  d13C corrected
  d13C_corrected <- amount_corrected
  # col 6 is depth, remember that ground surface was 1 m on the tape
  depth <- as.numeric(dC13_data[ dC13_data_rows, ][[6]]) / 100 - 1
  # assign run numbers using date
  analysis_date <- as.numeric(dC13_data[ dC13_data_rows, ][[1]])

  # put into dataframe
  d13C_depth <- data.frame(sample_ID = sample_ID,
                           sample_mass = sample_mass,
                           d15N = d15N,
                           depth = depth ,
                           d13C_corrected = d13C_corrected,
                           d13C_accuracy = d13C_accuracy,
                           d13C_precision = d13C_precision,
                           amount_corrected = amount_corrected,
                           percent_C = percent_C,
                           analysis_date = analysis_date
  )

  # there are some replicates, let's identify them and get
  # max and min values for each sample
  d13C_depth_means <- d13C_depth %>%
    group_by(factor(depth)) %>%
    summarize(mean_d13C_corrected = mean(d13C_corrected, na.rm = TRUE),
              max_d13C_corrected = max(d13C_corrected),
              min_d13C_corrected = min(d13C_corrected),
              diff_d13C_corrected = max_d13C_corrected - min_d13C_corrected) %>%
    mutate(depth = as.character(`factor(depth)`)) %>%
    select(-`factor(depth)`)

  # get the total station depths for the tape depths for dC13

  d13C_depth_means_total_station <-
    dplyr::left_join(d13C_depth_means,
                     tape_depths_interp,
                     by = c('depth' = 'tape'))

  # depth below surf from total station
  surf <- 100.693213   # NE_SEC_TAPE_1
  d13C_depth_means_total_station$total_station_depth_below_surface <-
    surf - as.numeric(d13C_depth_means_total_station$total_station)

  # for joining with other data
  d13C_depth_means_total_station$tape_depth <-
    d13C_depth_means_total_station$depth

  d13C_depth_means_total_station$depth <- NULL

  d13C_depth_means_total_station$total_station_depth_below_surface_rounded <-
    round(d13C_depth_means_total_station$total_station_depth_below_surface, 2)
  # that's all for d13C for now

  # Read in the other geoarch data

  burnt_earth_C3 <- read.csv("data/geoarchaeology_data/C3_burnt_earth_by_spit.csv")
  heated_per_litre_C3 <- read.csv("data/geoarchaeology_data/C3_heated_per_litre_by_spit.csv")
  burnt_chert_C3 <- read.csv("data/geoarchaeology_data/C3_burnt_chert_by_spit.csv")
  charcoal_per_litre_C3 <- read.csv("data/geoarchaeology_data/C3_charcoal_per_litre_by_spit.csv")
  charcoal_C3 <- read.csv("data/geoarchaeology_data/C3_charcoal_by_spit.csv")
  artefacts_per_litre_C3 <- read.csv("data/geoarchaeology_data/C3_artefacts_per_litre_by_spit.csv")
  artefacts_C3 <- read.csv("data/geoarchaeology_data/C3_artefacts_by_spit.csv")

  EL <-    cleaned_rotated_points_in_main_excavation_area[grepl("EL_",
                   cleaned_rotated_points_in_main_excavation_area$Description), ]

  EL$depth_below_surface <- surf - EL$Elevation


  # get spit depths from EL
  C3_spit_depths <-
    EL %>%
    filter(square == "C3") %>%
    group_by(spit) %>%
    dplyr::summarise(mean_depth_below_surface = mean(depth_below_surface)) %>%
    arrange(as.numeric(spit)) %>%
    ungroup %>%
    mutate(spit = as.numeric(spit)) %>%
    na.omit

  # we do need to interpolate a few missing spits
  xout <- seq(min(C3_spit_depths$spit),
              max(C3_spit_depths$spit),
              1)
  C3_spit_depths_interp <-
    data.frame(do.call(cbind,
               approxExtrap(C3_spit_depths$spit,
               C3_spit_depths$mean_depth_below_surface,
               xout = xout)))

  names(C3_spit_depths_interp) <- c("spit", "depth_below_surf")

  # first few spits are -ve, so lets fix that
  C3_spit_depths_interp[1:3, 2] <-  c(0, 0.05, 0.1)

  # combine all the things from C3
  things_in_C3 <-
    left_join(burnt_earth_C3,
              C3_spit_depths_interp,  # join with total station depths
              by = c("Spit" = "spit")) %>%
    left_join(.,
              heated_per_litre_C3) %>%
    left_join(.,
              burnt_chert_C3) %>%
    left_join(.,
              charcoal_per_litre_C3) %>%
    left_join(.,
              charcoal_C3) %>%
    left_join(.,
              artefacts_per_litre_C3) %>%
    left_join(.,
              artefacts_C3)

  # get rid of NA rows
  things_in_C3 <- things_in_C3[!is.na(things_in_C3$Spit),]

  # We also want mag sus data in here, join by depth,
  # since that's how the mag sus samples were recorded

  # round depths so we can get a match

  things_in_C3$depth_below_surf <-  round(things_in_C3$depth_below_surf, 2)

  mag_sus_B2_total_station$total_station_depth_m <- round(mag_sus_B2_total_station$total_station_depth_m, 2)

  # interpolate depth values for every 0.01 below the surface
  # for each data col in things_in_C3
  # don't interpolat data, just have gaps
  depths_0.01 <- seq(0,
                     max(d13C_depth_means_total_station$total_station_depth_below_surface),
                     0.01)

  # just match the rows and allow gaps
  all_rows <- data.frame(depths = depths_0.01)
  all_rows <- left_join(all_rows,
                        things_in_C3,
                        by = c("depths" = "depth_below_surf"))

  # subset to only depths that we have geoarch data for
  # all_rows <- all_rows[depths_0.01 <= max(things_in_C3$depth_below_surf), ]

  # add mag sus data
  all_rows <-
    left_join(all_rows,
              mag_sus_B2_total_station,
              by = c("depths" = "total_station_depth_m"))

  # and now add d13C data to all of that
  all_rows <-
    left_join(all_rows,
              d13C_depth_means_total_station,
              by = c("depths" = "total_station_depth_below_surface_rounded"))

  # straigraphic panel plot
  depth <- all_rows$depths
  all_rows$Charcoal.Litre.log <- log(all_rows$Charcoal.Litre)
  all_rows$Charcoal.Litre.log[all_rows$Charcoal.Litre.log == -Inf] <- NA

  # subset variables for plotting
  plotting_data <- select(all_rows,
                          # Burnt.Earth,
                          # Heated.litre,
                          Burnt.Chert,
                          # Charcoal.Litre,
                          Charcoal.Litre.log,
                          # Charcoal.g,
                          # Artefacts.Litre,
                          Artefacts,
                          Xlf..m.3.kg.,
                          mean_d13C_corrected
                          )


plt <- Stratiplot(depth ~ .,
                  varTypes = "absolute",
                  data = plotting_data,
                  type = c("l","g"),
                  col = "black",
                  lwd = 2)


}



