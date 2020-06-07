### function to move output files to a species-specific subfolder.
# you need a path to a directory that contains the files you want to move (in_path),
# an output path that contains the directory you want to move the files to (out_path),
# and a string pattern that identifies the files (e.g. "ahor" is on all file names attributed to species from Argosarchus horridus)
# the string pattern just needs to be a short string contained. I append a regex expression to make it only search for files.
library(fs)
library(purrr)
library(magrittr)
move_to_species <- function(in_path, out_path, pattern) {
  
  # check if subfolder exists. If not, make it
  if (!fs::dir_exists(out_path)) {
    fs::dir_create(out_path)
    
    } else
    print("Directory already there.")
  
  # list all files in the input directory with their full paths
  in_files_full <-
    fs::dir_ls(in_path, type = "file", regexp = pattern)
  
  # files without their full paths
  if (
    fs::dir_ls(in_path, type = "file", regexp = pattern) %>% 
      fs::path_file() %>% 
      length() > 0
  ) {
    in_files <- fs::dir_ls(in_path, type = "file", regexp = pattern) %>% 
     fs::path_file()
    
    # append the file names to the out_path
    out_files <- fs::path(out_path, in_files)
    
    # move the files
    fs::file_move(in_files_full, out_files)
    
  } else print("No files to move.")
  
  if (
    fs::dir_ls(in_path, regexp = pattern) %>% 
    fs::path_file() %>% 
    length() > 0
  ) {
   in_dir <- fs::dir_ls(in_path, regexp = pattern)
   
   in_dir_base <- in_dir %>% 
     fs::path_file()
   
   out_dirs <- fs::path(out_path, in_dir_base)
   
   purrr::map2(in_dir, out_dirs, ~fs::dir_copy(.x, .y, overwrite = TRUE))
   
   # remove original directories
   purrr::map(in_dir, fs::dir_delete)
   
  } else print("No directories to move.")
  
}