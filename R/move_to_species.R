### function to move output files to a species-specific subfolder.
# you need a path to a directory that contains the files you want to move (in_path),
# an output path that contains the directory you want to move the files to (out_path),
# and a string pattern that identifies the files (e.g. "ahor" is on all file names attributed to species from Argosarchus horridus)

move_to_species <- function(in_path, out_path, pattern) {
  
  # check if subfolder exists. If not, make it
  if (!dir.exists(out_path)) {
    dir.create(out_path)
  } else
    print("Directory already there.")
  
  # list all files in the input directory with their full paths
  in_files_full <-
    list.files(in_path, pattern = pattern, full.names = TRUE)
  
  # list all files in the input directory with only the file names
  in_files <- list.files(in_path, pattern = pattern)
  
  # append the file names to the out_path
  out_files <- file.path(out_path, in_files)
  
  # move the files
  file.rename(in_files_full, out_files)
  
}