utils::globalVariables(c("words_key","pos_key"))
#' Bind a column of synonyms to a dataframe of words
#'
#' @param tbl dataframe to bind synonyms to
#' @param words A vector of UNIQUE words to return synonyms for
#' @param pos Optional vector of part of speech tags corresponding to \code{words}. Returns synonyms for all parts of speech available if set to NULL.  Will ignore elements passed as \code{NA} and will return synonyms for all parts of speech
#' @param unnest Should list column of synonym vectors be unnested with \code{tidyr::unnest}
#' @param min_rel_level Getting synonyms from thesaurus.com will return a relevance rank (1 being most relevant); this param sets the minumum rank to return
#' @param kill_ALL_conns_on_exit If a url can't be found, a connection will linger creating a warning message about closing unused connections.  This parameter will close these connections w/o warning; set to FALSE if you are using other connections that you dont want closed.
#' @return A list of equal length to \code{words}.  Each list is a character vector of the synonyms found. If no synomyms found then NA will be returned for that word part of speech pair
#' @examples
#'my_df <- data.frame(my_words=c("test","test"),
#'                    my_pos=c("noun","verb"),
#'                    stringsAsFactors = FALSE)
#'
#'bind_synonyms(my_df, my_words, my_pos)
#'bind_synonyms_(my_df, "my_words", "my_pos")


#' @export
bind_synonyms_ <- function(tbl, words, pos=NULL, unnest=FALSE, min_rel_level=1, kill_ALL_conns_on_exit=TRUE) {
  key_df <- dplyr::tibble(words_key = tbl[[words]])

  tbl_names_in  <- names(tbl)
  tbl_names_out <- c(tbl_names_in, "synonyms")
  tbl_names_mod <- tbl_names_in
  tbl_names_mod[which(tbl_names_in==words)] <- "words"

  if(!is.null(pos)) {
    key_df$pos_key <-tbl[[pos]]
    tbl_names_mod[which(tbl_names_in==pos)] <- "pos"
  }

  tbl <- purrr::set_names(tbl, tbl_names_mod)

  key_df <- key_df %>%
    unique()

  if(!is.null(pos)) {
    syn_df <- key_df %>%
      dplyr::mutate(synonyms = get_syns(words_key, pos_key, min_rel_level=min_rel_level,
                                        kill_ALL_conns_on_exit=kill_ALL_conns_on_exit))
    out_df <- tbl %>%
      dplyr::left_join(syn_df, by=c("words"="words_key", "pos"="pos_key"))

  } else {
    syn_df <- key_df %>%
      dplyr::mutate(synonyms = get_syns(words_key, min_rel_level=min_rel_level,
                                        kill_ALL_conns_on_exit=kill_ALL_conns_on_exit))
    out_df <- tbl %>%
      dplyr::left_join(syn_df, by=c("words"="words_key"))
  }

  out_df <- purrr::set_names(out_df, tbl_names_out)

  if(unnest){
    out_df <- tidyr::unnest_(out_df, "synonyms")
  }

  out_df
}

#' @rdname bind_synonyms_
#' @export
bind_synonyms <- function(tbl, words, pos=NULL, unnest=FALSE, min_rel_level=1, kill_ALL_conns_on_exit=TRUE) {
  words   <- as.character(substitute(words))
  pos_str <- substitute(pos)
  if (!is.null(pos_str)) pos_str <- as.character(pos_str)

  bind_synonyms_(tbl, words, pos_str, unnest, min_rel_level, kill_ALL_conns_on_exit)
}
