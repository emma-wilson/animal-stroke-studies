searchPubMed <- function(searchDate, lastSearchDate){
  
  # PubMed terms as string
  pmterms <- '(stroke[MeSH Terms] OR stroke[All Fields] OR ischemia[MeSH Terms] OR ischemia[All Fields] OR cerebrovascular[All Fields] OR "middle cerebral artery"[MeSH Terms] OR "middle cerebral artery"[All Fields] OR MCA[All Fields] OR ACA[All Fields] OR "anterior cerebral artery"[MeSH Terms] OR "anterior cerebral artery"[All Fields] OR MCAO[All Fields]) NOT (coronary[All Fields] OR myocardial[All Fields])'
  #put together into search string
  pmsearch <- paste0(pmterms,' AND ("',lastSearchDate,'"[Date - Create] : "',searchDate,'"[Date - Create])')
  
  # Run search via pubmedtools
  metaData <- RetriveMetaDataFromSearch(pmsearch,
                                        outputFilename = "covidpubmed",
                                        columns = c("pmid","pmcid", "journal",
                                                    "journalCountry",
                                                    "publicationYear",
                                                    "funders",
                                                    "authors",
                                                    "affiliations",
                                                    "title",
                                                    "abstract","isbn",
                                                    "volume",
                                                    "issue",
                                                    "pages", "keywords","doi"))
  
  # Fix author column
  metaData$authors <- gsub("\\|\\|", "; ", metaData$authors)
  
  # Rename and select columns. Create unique ID
  pubmed <- metaData %>%
    mutate(DB = "pubmed",
           UniqueIdStr = paste0(DB,"-",pmid),
           Language = NA) %>%
    select(Title = title,
           Author = authors,
           Journal = journal,
           DOI = doi,
           Abstract = abstract,
           Accession = pmid,
           PMCID = pmcid,
           Year = publicationYear,
           AuthorAddress = affiliations,
           UniqueIdStr,
           Volume = volume,
           Number = issue,
           Pages = pages,
           Keywords = keywords,
           ISBN = isbn,
           JournalCountry = journalCountry,
           Language,
           DB)
  
  # Return pubmed data
  return(pubmed)
  
}