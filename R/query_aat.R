#' Query the Getty AAT SPARQL Service
#' 
#'Function to query the Getty Art & Architecture Thesaurus. 
#'
#' @return A dataframe of \code{term} (the string provided),
#' \code{aat_term} (the matched term from AAT), \code{aat_id} (the id of the term, prefixed with 'aat:'), \code{aat_note} (Notes of the term), \code{aat_parents} (parent categories of the term).
#'
#' @param term String to check against the AAT
#' @param query_type Which query to use, one of: 'direct', 'anylabel', or 'fulltext'
#' @param keywords_string String to extract keywords from to try to get a better match (optional)
#' 
#'
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_squish
#' @importFrom tokenizers tokenize_words
#' @importFrom stopwords stopwords
#' @importFrom utils URLencode
#'  
query_aat <- function(term = NA, query_type = NA, keywords_string = NA){
  
  getty_url <- "http://vocab.getty.edu/sparql.json?query="
  
  if (is.na(term) || term == ""){
    stop("term can not be empty")
  }
  
  if (is.na(query_type) || query_type == ""){
    stop("query_type can not be empty")
  }
  
  query_type_options <- c('direct', 'anylabel', 'fulltext', 'widesearch')
  
  if (!query_type %in% query_type_options){
    stop("invalid query_type")
  }
  
  #results data frame----
  results <- data.frame(matrix(nrow = 0, ncol = 5, data = NA))
  names(results) <- c("term", "aat_term", "aat_id", "aat_note", "aat_parents")
  
  if (query_type == "direct"){
    #direct match ----
    #Explicit Engligh
    getty_query_template <- "select 
            	?Subject ?Term ?ScopeNote ?Parents
            		{
            			?Subject gvp:prefLabelGVP/xl:literalForm \"%s\"@en; 
            			skos:inScheme aat:; 
            			gvp:prefLabelGVP [xl:literalForm ?Term].
            			optional {
            				?Subject skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]
            			}
            			optional {
            				?Subject gvp:parentString ?Parents
            			}
            		}"
    
    #Replace search string in query
    getty_query <- stringr::str_squish(sprintf(getty_query_template, term))

    #URLEncode the query
    getty_query_encoded <- utils::URLencode(getty_query, reserved = FALSE)
    
    json <- try(jsonlite::fromJSON(paste0(getty_url, getty_query_encoded), flatten = TRUE), silent = TRUE)
    
    if (class(json) != "try-error"){
      for (i in seq(1, dim(json$results$bindings)[1])){
        results <- rbind(results, 
                         cbind("term" = term, 
                               "aat_term" = json$results$bindings$Term.value,
                               "aat_id" = paste0("aat:", stringr::str_replace(json$results$bindings$Subject.value, "http://vocab.getty.edu/aat/", "")), 
                               "aat_note" = json$results$bindings$ScopeNote.value,
                               "aat_parents" = json$results$bindings$Parents.value)
        )
      }
    }else{
      getty_query_template <- "select 
            	?Subject ?Term ?ScopeNote ?Parents
            		{
            			?Subject gvp:prefLabelGVP/xl:literalForm \"%s\"; 
            			skos:inScheme aat:; 
            			gvp:prefLabelGVP [xl:literalForm ?Term].
            			optional {
            				?Subject skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]
            			}
            			optional {
            				?Subject gvp:parentString ?Parents
            			}
            		}"
      
      #Replace search string in query
      getty_query <- stringr::str_squish(sprintf(getty_query_template, term))
      
      #URLEncode the query
      getty_query_encoded <- URLencode(getty_query, reserved = FALSE)
      
      json <- try(jsonlite::fromJSON(paste0(getty_url, getty_query_encoded), flatten = TRUE), silent = TRUE)
      
      if (class(json) != "try-error"){
        for (i in seq(1, dim(json$results$bindings)[1])){
          results <- rbind(results, 
                           cbind("term" = term, 
                                 "aat_term" = json$results$bindings$Term.value,
                                 "aat_id" = paste0("aat:", stringr::str_replace(json$results$bindings$Subject.value, "http://vocab.getty.edu/aat/", "")), 
                                 "aat_note" = json$results$bindings$ScopeNote.value,
                                 "aat_parents" = json$results$bindings$Parents.value)
          )
        }
      }
    }
  }else if (query_type == "anylabel"){
    #anylabel search ----
    term_clean <- stringr::str_replace_all(term, "'s", "")
    term_clean <- stringr::str_replace_all(term_clean, "'", "")
    
    getty_query_template <- "select 
                    ?Subject ?Term ?ScopeNote ?Parents
                    	{
                            ?Subject rdfs:label \"%s\"@en;
                            skos:inScheme aat:; 
                            gvp:prefLabelGVP [xl:literalForm ?Term];
                            optional {
                        			?Subject gvp:parentString ?Parents
                        		}
                      			optional {
                      				?Subject skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]
                      			}
                        }"
    
    #Replace search string in query
    getty_query <- stringr::str_squish(sprintf(getty_query_template, term_clean))
    
    #URLEncode the query
    getty_query_encoded <- URLencode(getty_query, reserved = FALSE)
    
    json <- try(jsonlite::fromJSON(paste0(getty_url, getty_query_encoded), flatten = TRUE), silent = TRUE)
    
    if (class(json) != "try-error"){
      for (i in seq(1, dim(json$results$bindings)[1])){
        results <- rbind(results, 
                         cbind("term" = term, 
                               "aat_term" = json$results$bindings$Term.value,
                               "aat_id" = paste0("aat:", stringr::str_replace(json$results$bindings$Subject.value, "http://vocab.getty.edu/aat/", "")), 
                               "aat_note" = json$results$bindings$ScopeNote.value,
                               "aat_parents" = json$results$bindings$Parents.value)
        )
      }
    }
  }else if (query_type == "fulltext"){
    #fulltext search ----
    
    #Cleanup term
    term_clean <- stringr::str_replace_all(term, "'s", "")
    term_clean <- stringr::str_replace_all(term_clean, "'", "")
    
    getty_query_template <- "select 
                        ?Subject ?Term ?Parents ?ScopeNote {
              	          ?Subject a skos:Concept; luc:term \"%s\"; 
              	          skos:inScheme aat: ; 
              	          gvp:prefLabelGVP [xl:literalForm ?Term]. 
              	          optional {
              	              ?Subject gvp:parentString ?Parents
              	              }
              	          optional {
              	              ?Subject skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]
              	              }
                        }"
    
    #Replace search string in query
    getty_query <- stringr::str_squish(sprintf(getty_query_template, term_clean))
    
    #URLEncode the query
    getty_query_encoded <- URLencode(getty_query, reserved = FALSE)
    
    json <- try(jsonlite::fromJSON(paste0(getty_url, getty_query_encoded), flatten = TRUE), silent = TRUE)
    
    if (class(json) != "try-error"){
      for (i in seq(1, dim(json$results$bindings)[1])){
        results <- rbind(results, 
                         cbind("term" = term, 
                               "aat_term" = json$results$bindings$Term.value,
                               "aat_id" = paste0("aat:", stringr::str_replace(json$results$bindings$Subject.value, "http://vocab.getty.edu/aat/", "")), 
                               "aat_note" = json$results$bindings$ScopeNote.value,
                               "aat_parents" = json$results$bindings$Parents.value)
        )
      }
    }
    
    #fulltext keywords ----
    keywords_tokenized <- unique(tokenizers::tokenize_words(keywords_string, stopwords = stopwords::stopwords("en"), simplify = TRUE, strip_punct = TRUE, strip_numeric = TRUE, lowercase = TRUE))
    
    if (length(keywords_tokenized) > 0){
      for (i in seq(1, length(keywords_tokenized))){
        
        getty_query_template <- "select 
                        ?Subject ?Term ?Parents ?ScopeNote {
              	          ?Subject a skos:Concept; luc:term \"%s\"; 
              	          skos:inScheme aat: ; 
              	          gvp:prefLabelGVP [xl:literalForm ?Term]. 
              	          optional {
              	              ?Subject gvp:parentString ?Parents
              	              }
              	          optional {
              	              ?Subject skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]
              	              }
                        }"
        
        #Replace search string in query
        string_to_query <- paste0(term_clean, " AND ", keywords_tokenized[i])
        getty_query <- stringr::str_squish(sprintf(getty_query_template, string_to_query))
        
        #URLEncode the query
        getty_query_encoded <- URLencode(getty_query, reserved = FALSE)
        
        json <- try(jsonlite::fromJSON(paste0(getty_url, getty_query_encoded), flatten = TRUE), silent = TRUE)
        
        if (class(json) != "try-error"){
          for (i in seq(1, dim(json$results$bindings)[1])){
            results <- rbind(results, 
                             cbind("term" = term, 
                                   "aat_term" = json$results$bindings$Term.value,
                                   "aat_id" = paste0("aat:", stringr::str_replace(json$results$bindings$Subject.value, "http://vocab.getty.edu/aat/", "")), 
                                   "aat_note" = json$results$bindings$ScopeNote.value,
                                   "aat_parents" = json$results$bindings$Parents.value)
            )
          }
        }
      }
    }
    
    
  }else if (query_type == "widesearch"){
    #widesearch----
    
    #Cleanup term
    term_clean <- stringr::str_replace_all(term, "'s", "")
    term_clean <- stringr::str_replace_all(term_clean, "'", "")
    
    getty_query_template <- "select ?Subject ?Term ?Parents ?ScopeNote ?Type (coalesce(?Type1,?Type2) as ?ExtraType) {
            ?Subject luc:term \"%s\"; a ?typ.
            ?typ rdfs:subClassOf gvp:Subject; rdfs:label ?Type.
            filter (?typ != gvp:Subject)
            optional {?Subject gvp:placeTypePreferred [gvp:prefLabelGVP [xl:literalForm ?Type1]]}
            optional {?Subject gvp:agentTypePreferred [gvp:prefLabelGVP [xl:literalForm ?Type2]]}
            optional {?Subject gvp:prefLabelGVP [xl:literalForm ?Term]}
            optional {?Subject gvp:parentStringAbbrev ?Parents}
            optional {?Subject skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]}}"
    
    #Replace search string in query
    getty_query <- stringr::str_squish(sprintf(getty_query_template, term_clean))
    
    #URLEncode the query
    getty_query_encoded <- URLencode(getty_query, reserved = FALSE)
    
    json <- try(jsonlite::fromJSON(paste0(getty_url, getty_query_encoded), flatten = TRUE), silent = TRUE)
    
    if (class(json) != "try-error"){
      for (i in seq(1, dim(json$results$bindings)[1])){
        results <- rbind(results, 
                         cbind("term" = term, 
                               "aat_term" = json$results$bindings$Term.value,
                               "aat_id" = paste0("aat:", stringr::str_replace(json$results$bindings$Subject.value, "http://vocab.getty.edu/aat/", "")), 
                               "aat_note" = json$results$bindings$ScopeNote.value,
                               "aat_parents" = json$results$bindings$Parents.value)
        )
      }
    }
    
    
    #fulltext keywords ----
    keywords_tokenized <- unique(tokenizers::tokenize_words(keywords_string, stopwords = stopwords::stopwords("en"), simplify = TRUE, strip_punct = TRUE, strip_numeric = TRUE, lowercase = TRUE))
    
    if (length(keywords_tokenized) > 0){
      for (i in seq(1, length(keywords_tokenized))){
        
        getty_query_template <- "select ?Subject ?Term ?Parents ?ScopeNote ?Type (coalesce(?Type1,?Type2) as ?ExtraType) {
            ?Subject luc:term \"%s\"; a ?typ.
            ?typ rdfs:subClassOf gvp:Subject; rdfs:label ?Type.
            filter (?typ != gvp:Subject)
            optional {?Subject gvp:placeTypePreferred [gvp:prefLabelGVP [xl:literalForm ?Type1]]}
            optional {?Subject gvp:agentTypePreferred [gvp:prefLabelGVP [xl:literalForm ?Type2]]}
            optional {?Subject gvp:prefLabelGVP [xl:literalForm ?Term]}
            optional {?Subject gvp:parentStringAbbrev ?Parents}
            optional {?Subject skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]}}"
        
        #Replace search string in query
        string_to_query <- paste0(term_clean, " AND ", keywords_tokenized[i])
        getty_query <- stringr::str_squish(sprintf(getty_query_template, string_to_query))
        
        #URLEncode the query
        getty_query_encoded <- URLencode(getty_query, reserved = FALSE)
        
        json <- try(jsonlite::fromJSON(paste0(getty_url, getty_query_encoded), flatten = TRUE), silent = TRUE)
        
        if (class(json) != "try-error"){
          for (i in seq(1, dim(json$results$bindings)[1])){
            results <- rbind(results, 
                             cbind("term" = term, 
                                   "aat_term" = json$results$bindings$Term.value,
                                   "aat_id" = paste0("aat:", stringr::str_replace(json$results$bindings$Subject.value, "http://vocab.getty.edu/aat/", "")), 
                                   "aat_note" = json$results$bindings$ScopeNote.value,
                                   "aat_parents" = json$results$bindings$Parents.value)
            )
          }
        }
      }
    }
  }
  
  #Done, return dataframe with results
  return(results)
}
