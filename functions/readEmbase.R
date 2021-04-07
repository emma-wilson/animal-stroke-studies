readEmbase <- function(searchfolder, pattern){
  
  # List all embase file names
  file_list <- list.files(path=searchfolder, pattern=pattern)
  # Add the file path to each file
  path_list <- paste0(searchfolder,"/",file_list)
  
  # Function to read in and format all embase files in path_list
  #Returns a list called 'loc'
  loc = lapply(path_list, function(i){
    # Read xlsx files
    x = read_xlsx(i, col_names = T, skip = 1)
    # Format so columns match
    x = formatEmbase(x)
    # Return data
    x
  })
  
  # Merge data from all embase files into one dataframe
  embase =  rbindlist(loc)
  
  # Filter out accidental duplicate records
  embase <- unique(embase)
  
}