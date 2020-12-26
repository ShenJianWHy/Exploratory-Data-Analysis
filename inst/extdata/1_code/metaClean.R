metaClean <- function(datIn, metaInstructions) {
  
  # First get the actual class types and check if it matches before parsing
  origClassess <- lapply(datIn, class) %>% unlist() 
  origClassess <- tibble(variable  = names(origClassess),
                         origClass = origClassess %>% tolower() %>% ifelse(.=="integer",yes = "numeric", .))
  varTypeInstructions <- metaInstructions %>% 
    left_join(origClassess) %>%
    filter(origClass != varType) %>%
    select(-origClass)
  
  # First convert all variables that needs to be converted to character type, since it is easiest to manipulate from there
  datIn <- datIn %>% mutate_at(vars(one_of(varTypeInstructions$variable)), funs(as.character))
  
  # Find the different types of conversions applicable
  types <- unique(varTypeInstructions$varType)

  for (tp in types) {
    vars <- metaInstructions$variable[metaInstructions$varType == tp]
    if (tp == "numeric") {
      datIn <- datIn %>% mutate_at(vars(one_of(vars)), funs(as.numeric))
    } else if (tp == "date") {
      datIn <- datIn %>% mutate_at(vars(one_of(vars)), funs(parsedate::parse_date))
    } 
  }
  return(datIn)
}

