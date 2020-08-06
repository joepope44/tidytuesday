# searches sub-directories in TidyTuesday and find list of png files 
plot_list <- list.files(pattern = "\\.png$", recursive = TRUE)

current_folder <- here::here()

new_folder <- here::here("plots")

# save all png files to the plots folder 
file.copy(file.path(current_folder,plot_list), new_folder)

