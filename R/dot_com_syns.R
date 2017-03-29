utils::globalVariables(c("relevance","."))
#' Get synonyms for UNIQUE word part of speech pairs
#
#' @param words A vector of UNIQUE words to return synonyms for
#' @param pos Optional vector of part of speech tags corresponding to \code{words}. Returns synonyms for all parts of speech available if set to NULL.  Will ignore elements passed as \code{NA} and will return synonyms for all parts of speech
#' @param min_rel_level Getting synonyms from thesaurus.com will return a relevance rank (1 being most relevant); this param sets the minumum rank to return
#' @param source literally does nothing right now. plan to add more sources for synonyms in future
#' @param kill_ALL_conns_on_exit If a url can't be found, a connection will linger creating a warning message about closing unused connections.  This parameter will close these connections w/o warning; set to FALSE if you are using other connections that you dont want closed.
#' @return A list of equal length to \code{words}.  Each list is a character vector of the synonyms found. If no synomyms found then NA will be returned for that word part of speech pair
#' @examples
#' get_syns(c("test","test"),c("noun","verb"))
#' @importFrom magrittr "%>%"

#' @export
get_syns <- function(words, pos=NULL, min_rel_level=1, source="thesaurus.com", kill_ALL_conns_on_exit=TRUE) {
  if(kill_ALL_conns_on_exit) on.exit(closeAllConnections())
  out_names     <- words
  url_word_tail <- paste0("/",purrr::map(words, ~URLencode(.x)))
  url_pos_tail  <- rep(NA, length(url_word_tail))

  if(!is.null(pos)) {
    if(length(words) != length(pos)) stop("words and pos must be same length")
    url_pos_tail[!is.na(pos)] <- paste0(url_word_tail[!is.na(pos)],"/",pos[!is.na(pos)])
    out_names[!is.na(pos)]    <- paste0(out_names[!is.na(pos)], "_", pos[!is.na(pos)])
  }

  if(length(unique(out_names)) != length(out_names)) stop("word part of speech pairings must be UNIQUE!!")

  purrr::map2(url_word_tail, url_pos_tail, function(.x,.y){
    syn_df <- NULL
    if(!is.na(.y)) {
      syn_df <- safe_quiet_get_thesaurus.com_syns_df(.y)
    }
    if(is.null(syn_df)) {
      syn_df <- safe_quiet_get_thesaurus.com_syns_df(.x)
    }
    if(!is.null(syn_df)) {
      syn_df %>%
        dplyr::filter(relevance >= min_rel_level) %>%
        .[["synonym"]]
    } else {
      NA_character_
    }
  }) %>%
    purrr::set_names(out_names)
}
