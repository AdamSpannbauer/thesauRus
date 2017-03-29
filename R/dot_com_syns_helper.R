utils::globalVariables(c("synonym","score_rank"))
#' Get synonyms from thesaurus.com with their relevance scores
#
#' @param word_slash_pos word and part of speech passed as <word/pos> if not providing pos no slash is needed after word
#' @return A tibble with 2 columns: synonym & relevance
#' @examples
#' get_thesaurus.com_syns_df("fly")
#' get_thesaurus.com_syns_df("fly/verb")

#' @export
get_thesaurus.com_syns_df <- function(word_slash_pos) {
  url <- paste0("http://www.thesaurus.com/browse/", word_slash_pos)
  selector <-
    "div.filters > div.relevancy-block > div.relevancy-list > ul > li > a"
  syn_nodes <- xml2::read_html(url) %>%
    rvest::html_nodes(selector)
  syns <- rvest::html_attr(syn_nodes, "href") %>%
    stringr::str_split("/") %>%
    purrr::map_chr(~tail(.x,1) %>%
                     URLdecode())
  rels <- rvest::html_attr(syn_nodes, "data-category") %>%
    stringr::str_extract_all("relevant-\\d") %>%
    purrr::map_chr(1) %>%
    stringr::str_extract_all("\\d+$") %>%
    purrr::map_chr(1) %>%
    as.numeric()

  out_df <- dplyr::tibble(synonym=syns, relevance=rels)
  rev_rel <- dplyr::tibble(relevance  = unique(out_df$relevance),
                           score_rank = rank(-unique(out_df$relevance)))
  out_df %>%
    dplyr::left_join(rev_rel, by="relevance") %>%
    dplyr::select(synonym, relevance=score_rank)
}

safe_quiet_get_thesaurus.com_syns_df <- dplyr::failwith(NULL, get_thesaurus.com_syns_df, quiet=TRUE)
