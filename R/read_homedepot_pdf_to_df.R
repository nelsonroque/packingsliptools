read_homedepot_pdf_to_df <- function(pdf_file, sort="first,last", verbose=F) {
  # read as pdf
  pdf_content <- pdf_data(pdf_file)

  # write for loop here
  page_data <- tibble()
  for(page in 1:length(pdf_content)) {
    # get first page
    cur_page <- pdf_content[[page]]

    purchaseorder_number <- cur_page %>%
      filter(between(x,360,370) & between(y, 90, 110)) %>%
      select(text) %>%
      as.vector(.) %>%
      sapply(., paste, collapse=' ') %>%
      toString(.) %>%
      toupper(.)

    customerorder_number <- cur_page %>%
      filter(between(x,360,370) & between(y, 80, 90)) %>%
      select(text) %>%
      as.vector(.) %>%
      sapply(., paste, collapse=' ') %>%
      toString(.) %>%
      toupper(.)

    order_date <- cur_page %>%
      filter(between(x,320,370) & between(y, 110, 130)) %>%
      select(text) %>%
      as.vector(.) %>%
      sapply(., paste, collapse=' ') %>%
      toString(.) %>%
      toupper(.)

    shipto_name <- cur_page %>%
      filter(between(x,30,120) & between(y, 200, 210)) %>%
      select(text) %>%
      as.vector(.) %>%
      sapply(., paste, collapse=' ') %>%
      toString(.) %>%
      toupper(.)

    soldto_name <- cur_page %>%
      filter(between(x,250,370) & between(y, 60, 70)) %>%
      select(text) %>%
      as.vector(.) %>%
      sapply(., paste, collapse=' ') %>%
      toString(.) %>%
      toupper(.)

    shipto_vector <- unlist(strsplit(shipto_name, " "))
    shipto_firstname <- toString(shipto_vector[1])
    shipto_lastname <- toString(tail(shipto_vector, 1))

    # create record for page
    export_data = tibble(page_index = page,
                         page_nrows = nrow(cur_page),
                         purchaseorder_number = purchaseorder_number,
                         customerorder_number = customerorder_number,
                         order_date = order_date,
                         shipto_fullname = shipto_name,
                         shipto_firstname = shipto_firstname,
                         shipto_lastname = shipto_lastname,
                         soldto_name = soldto_name)

    # save record to collector
    page_data <- bind_rows(page_data, export_data)

    # write debug info
    if(verbose) {
      print(paste0("Original Page: ", page))
      print(paste0("Purchase Order: ", purchaseorder_number))
      print(paste0("Customer Order: ", customerorder_number))
      print(paste0("Order Date: ", order_date))
      print(paste0("Sold To: ", soldto_name))
      print(paste0("Ship To: ", shipto_name))
      print("----------------------")
    }
  }

  if(sort == "first,last") {
    # arrange by first name and then last name
    sorted_data <- page_data %>%
      arrange(shipto_firstname, shipto_lastname) %>%
      mutate(sorted_index = row_number())
  } else {
    if(sort == "last,first") {
      # arrange by first name and then last name
      sorted_data <- page_data %>%
        arrange(shipto_lastname, shipto_firstname) %>%
        mutate(sorted_index = row_number())
    }
  }

  return(list(pdf_content = pdf_content, page_data = page_data, sorted_data = sorted_data))
}
