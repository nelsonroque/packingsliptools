write_pages_from_df <- function(pdf, df, wd) {

  # set directory for output files
  setwd(wd)

  # rewrite PDF in alphabetical order
  for(page_s in 1:nrow(df)) {
    cur_page_s <- df[page_s, ]
    pg_indx = cur_page_s$page_index
    sort_indx = cur_page_s$sorted_index
    po_num = cur_page_s$purchaseorder_number %>% as.character(.)
    ship_name = cur_page_s$shipto_fullname %>% as.character(.)

    # create output filename
    o_fn = paste0(sort_indx, "_", po_num, "_", ship_name, ".pdf")

    # write out page
    pdf_subset(pdf,
               pages = pg_indx,
               output = o_fn)

    # write page number
    print(paste0("Writing Page: ", page_s, " of ", nrow(df)))
    print("----------------------")
  }
}
