# Functions ---------------------------
read_excel_sheets <- function(sheet_name, path) {
x <-  readxl::read_excel(path = path,
                     sheet = sheet_name)
 }

read_pdf_data <-function(high_school_directory_pdfs){
  pdftools::pdf_data(high_school_directory_pdfs)
} 


remove_blank_pages <-function(nested_list_name) {
  nested_list_name[lapply(nested_list_name,nrow)!=0]
}

  
