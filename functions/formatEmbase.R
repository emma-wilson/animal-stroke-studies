formatEmbase <- function(x){
  
  # Set a name key with the columns to be renamed
  name_key = c(
    Title = "TI",
    Author = "AU",
    DOI = "DO",
    Abstract = "AB",
    AuthorAddress = "AD",
    Keywords = "KW",
    Accession = "UI",
    Year = "YR",
    Citation = "SO",
    JournalCountry = "CP",
    Language = "LG",
    ISBN = "IS",
    PMID = "PM"
  )
  
  # Set values for columns in the name key but not in the data frame as NA
  for (var in names(name_key)) {
    if (!(name_key[[var]] %in% names(x))) {
      name_key[var] <- NA
    }
  }
  
  # Get a vector of column names in the data frame
  cols <- names(name_key[!is.na(name_key)])
  
  # Rename columns
  for (nm in names(name_key)) {
    names(x)[names(x) == name_key[[nm]]] <- nm
  }
  
  # Create empty tibble of missing column names
  missing <- names(name_key[is.na(name_key)]) %>%
    map_dfr( ~tibble(!!.x := logical() ) )
  
  # Add missing columns populated with NA
  x <- bind_rows(missing, x)
  
  #Create columns for DB, Unique ID, Pages, Number, Volume and Journal
  x <- x %>%
    mutate(DB = "EMBASE") %>%
    mutate(UniqueIdStr = paste0(DB,"-",Accession)) %>%
    mutate(PMCID = NA) %>%
    mutate(Pages = str_extract(Citation, "(\\(pp.*?\\))")) %>%
    mutate(Number = str_extract(Citation, "\\..?[0-9]*.?\\(.?\\)")) %>% 
    mutate(Volume = str_extract(Citation, "\\..?[0-9]*.?\\(")) %>%
    mutate(Journal = str_extract(Citation, "[^\\.]+"))
  
  # Tidy data in Pages, Number, Volume, PMID and Author columns
  x$Pages <- sub("\\(pp", "", x$Pages)
  x$Pages <- sub("\\)", "", x$Pages)
  x$Number <- sub(".*\\(", "", x$Number)
  x$Number <- sub("\\)", "", x$Number)
  x$Volume <- sub("\\. ", "", x$Volume)
  x$Volume <- sub(" \\(", "", x$Volume)
  
  x$PMID <- sub(" \\[.*", "", x$PMID)
  x$PMID <- as.character(x$PMID)
  
  x$Author <- gsub("\r\n\r\n", "; ", x$Author)
  
  # Select relevant columns
  x <- x %>% 
    select(-Citation) %>%
    select(Title, Author, Journal, DOI, Abstract, Accession, PMCID, Year, AuthorAddress, 
           UniqueIdStr, Volume, Number, Pages, Keywords, ISBN, JournalCountry, Language, DB, PMID) 
  
  return(x)
  
}