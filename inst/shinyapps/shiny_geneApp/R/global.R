
#' Write a maf file
#' @description
#' function that returns a maf file
#' @param maf the maf data
#' @param basename the name of the file
#' @return the maf file
write_maf_file <- function(maf, basename = NULL){
  if(is.null(basename)){
    stop('Please provide a basename for output file.')
  }
  #write main maf file
  data.table::fwrite(
    x = data.table::rbindlist(list(maf@data, maf@maf.silent), use.names = TRUE, fill = TRUE),
    file = basename,
    sep = '\t',
    quote = FALSE,
    row.names = FALSE
  )
}

#' Write the gene summary of a maf file
#' @description
#' function that returns the gene summary a maf file
#' @param maf the maf data
#' @param basename the name of the file
#' @return the gene summary of the maf file
write_gene_summary <- function(maf, basename = NULL){
  if(is.null(basename)){
    stop('Please provide a basename for output file.')
  }
  #write gene summary
  write.table(x = maf, file = basename, sep = '\t', quote = FALSE, row.names = FALSE)
}

#' Write the sample summary of a maf file
#' @description
#' function that returns the sample summary a maf file
#' @param maf the maf data
#' @param basename the name of the file
#' @return the sample summary of the maf file
write_sample_summary <- function(maf, basename = NULL){
  if(is.null(basename)){
    stop('Please provide a basename for output file.')
  }
  #write sample summary
  write.table(x = maf, file = basename, sep = '\t', quote = FALSE, row.names = FALSE)
}

#' Write the maf summary of a maf file
#' @description
#' function that returns the maf summary a maf file
#' @param maf the maf data
#' @param basename the name of the file
#' @return the maf summary of the maf file
write_maf_summary <- function(maf, basename = NULL){
  if(is.null(basename)){
    stop('Please provide a basename for output file.')
  }
  #write summary
  write.table(x = maf, file = basename, sep = '\t', quote = FALSE, row.names = FALSE)
}

#' useless tooltip
#' @description
#' function that activates tooltips
#' use once!
#' @examples tooltip_inutile()
tooltip_inutile <- function() {
  shiny::fluidRow(
    shiny::tags$span(
      style = "display: none;",
      `data-toggle` = "tooltip",
      `data-placement` = "left",
      title = "A tooltip",
      shiny::icon("question-circle")
    )
  )
}

#' Check for column names
#' @description
#' check for accepted maf column names
#' @param name the column name to check
#' @return
#' the correct name or NULL
#' @examples check_names("nomeesempio")
check_names <- function(name) {
  Gene <- c("gene.refgene", "gene")
  Hugo_Symbol <- c("hugo_symbol")
  Chromosome <- c("chr","chrom", "chromosome")
  Ref <- c("reference_allele", "ref")
  Alt <- c("tumor_seq_allele2", "alt")
  VAF <- c("vaf", "t_vaf")
  Variant_Classification <- c("variant_classification", "func.refgene")
  Variant_Type <- c("variant_type", "exonicfunc.refgene")
  VARIANT_CLASS <- c("variant_class")
  Clinvar <- c("clin_sig", "clinvar")
  Depth <- c("depth", "t_depth")
  Start_Position <- c("start_position", "start")
  End_Position <- c("end_position", "end")
  Variation <- c("existing_variation", "aachange.refgene", "variation", "var")
  HGVSp <- c("hgvsp")
  Exon <- c("exon")
  Tumor_Sample_Barcode <- c("tumor_sample_barcode")
  n <- tolower(name)
  if (n %in% Gene){"Gene"}
  else if (n %in% Hugo_Symbol){"Hugo_Symbol"}
  else if (n %in% Chromosome){"Chromosome"}
  else if (n %in% Ref){"Reference_Allele"}
  else if (n %in% Alt){"Tumor_Seq_Allele2"}
  else if (n %in% VAF){"VAF"}
  else if (n %in% Variant_Classification){"Variant_Classification"}
  else if (n %in% Variant_Type){"Variant_Type"}
  else if (n %in% VARIANT_CLASS){"VARIANT_CLASS"}
  else if (n %in% Clinvar){"CLIN_SIG"}
  else if (n %in% Depth){"t_depth"}
  else if (n %in% Start_Position){"Start_Position"}
  else if (n %in% End_Position){"End_Position"}
  else if (n %in% Variation){"Existing_Variation"}
  else if (n %in% HGVSp){"HGVSp"}
  else if (n %in% Exon){"EXON"}
  else if (n %in% Tumor_Sample_Barcode){"Tumor_Sample_Barcode"}
  else {NULL}
}

#' Check for the custom column names
#' @description
#' check for the custom column names to display
#' @param name the column name to check
#' @return
#' the correct name or NULL
#' @examples check_custom_names("nomeesempio")
check_custom_names <- function(name) {
  if (name == "Gene"){"Gene"}
  else if (name == "Hugo_Symbol"){"HugoSymbol"}
  else if (name == "Chromosome"){"Chromosome"}
  else if (name == "Reference_Allele"){"Ref"}
  else if (name == "Tumor_Seq_Allele2"){"Alt"}
  else if (name == "VAF"){"VAF"}
  else if (name == "Variant_Classification"){"Classification"}
  else if (name == "Variant_Type"){"VariantType"}
  else if (name == "VARIANT_CLASS"){"VariantClass"}
  else if (name == "CLIN_SIG"){"Clinvar"}
  else if (name == "t_depth"){"Depth"}
  else if (name == "Start_Position"){"Start"}
  else if (name == "End_Position"){"End"}
  else if (name == "Existing_Variation"){"Variation"}
  else if (name == "HGVSp"){"HGVSp"}
  else if (name == "EXON"){"Exon"}
  else if (name == "Tumor_Sample_Barcode"){"SampleBarcode"}
  else {NULL}
}

#' make UI elements
#' @description
#' this function creates the inputs for the supplied vectors
#' @param x the vector
#' @param var the name of the vector
#' @param id the module's id where the inputs will be created
#' @param n the suffix of the filter
#' @param s the prfeix of the container module
#' @return the filter created according to the conditions in the function
#' @examples make_ui(vector, name, moduleid)
make_ui <- function(x, var, id, n, s) {
  filter_name <- paste(var, n, sep = "")
  #NUMERIC VECTORS
  if (is.numeric(x)) {
    rng <- range(x, na.rm = T)
    if (!(rng[1] == rng[2])){
      shiny::sliderInput(paste(s, shiny::NS(id, filter_name), sep = ""), var, min = rng[1], max = rng[2], value = rng)
    }
    else {NULL}
  }
  #FACTORS
  else if (is.factor(x)) {
    levs <- levels(factor(x, exclude = NULL))
    shinyWidgets::pickerInput(paste(s, shiny::NS(id, filter_name), sep = ""), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox = TRUE, size = 10), choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40)))
  }
  #CARACTER VECTORS AND ARRAYS
  else if ( is.array(x) || is.character(x)) {
    ### --- TEST VARIOUS INPUTS HERE --- ###
    levs <- kit::funique(x)

    #handling of NA values
    if (NA %in% levs){
      levs <- levs[!is.na(levs)]
      levs <- append(levs, "NA")
    }
    shinyWidgets::pickerInput(paste(s, shiny::NS(id, filter_name), sep = ""), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox = TRUE, size = 10, virtualScroll = TRUE ), choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40)))
  }
  #LOGICAL VECTORS
  else if (is.logical(x)) {
    if(all(is.na(x)) || all(is.null(x))) {NULL}
    else{
      levs <- levels(factor(x, exclude = NULL))
      shinyWidgets::pickerInput(paste(s, shiny::NS(id, filter_name), sep = ""), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox = TRUE, size = 10), choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40)))
    }
  }
  else {
    # Not supported
    NULL
  }
}

#' Filter function
#' @description
#' this function decides if the records in the table are to keep or not
#' @param x the vector to filter
#' @param val the values supplied from the inputs
#' @return a logical vector mapping the values to remove from the table
#' @examples filter_var(vector, inputvalues)
filter_var <- function(x, val) {
  #NUMERIC VECTORS
  if (is.numeric(x)) {
    if (val[1] == val[2]) {TRUE}
    else {
      !is.na(x) & x >= val[1] & x <= val[2]
    }
  }
  #FACTORS
  else if (is.factor(x)) {
    x %in% val
  }
  #CARACTER VECTORS AND ARRAYS
  else if (is.array(x) || is.character(x)) {
    filtered_vector <- c()
    for(value in x){
      if(value %in% val){
        filtered_vector <- append(filtered_vector, T)
      }
      else if (is.na(value) && "NA" %in% val){
        filtered_vector <- append(filtered_vector, T)
      }
      else{
        filtered_vector <- append(filtered_vector, F)
      }
    }
    filtered_vector
  }
  #LOGICAL VECTORS
  else if (is.logical(x)) {T}
  else {
    # No control, so don't filter
    TRUE
  }
}

#' Filter function for multi page
#' @description
#' this function decide if the values in the table are to keep or not
#' the difference with the normalfuncion is that now NA values for numeric columns are not exclusive
#' @param x the vector to filter
#' @param val the values supplied from the inputs
#' @return a logical vector mapping the values to remove from the table
#' @examples filter_var_multi(vector, inputvalues)
filter_var_multi <- function(x, val) {
  #NUMERIC VECTORS
  if (is.numeric(x) ) {
    if (val[1] == val[2]) {TRUE}
    else {
      is.na(x) | (x >= val[1] & x <= val[2])
    }
  }
  #FACTORS
  else if (is.factor(x)) {
    x %in% val
  }
  #CARACTER VECTORS AND ARRAYS
  else if (is.array(x) || is.character(x)) {
    filtered_vector <- c()
    for(value in x){
      if(value %in% val){
        filtered_vector <- append(filtered_vector, T)
      }
      else if (is.na(value) && "NA" %in% val){
        filtered_vector <- append(filtered_vector, T)
      }
      else{
        filtered_vector <- append(filtered_vector, F)
      }
    }
    filtered_vector
  }
  #LOGICAL VECTORS
  else if (is.logical(x)) {T}
  else {
    # No control, so don't filter
    TRUE
  }
}

#' Create a toggle element
#' @description
#' used to create a toggle element in the ui to collapse well panels
#' @param id the button id
#' @param target the well panel id
#' @param text the text to display
#' @return the toggle row
#' @examples toggle_panel(toggleid, targetid, text)
toggle_panel <- function(id, target, text) {
  shiny::tags$div(
    class = "toggle_panel",
    shiny::tags$span(
      class = "toggle_panel_text",
      text
    ),
    shiny::tags$a(
      id = id,
      class = "toggle_well_link",
      `data-toggle` = "collapse",
      `data-target` = paste("#", target, sep = ""),
      shiny::icon("chevron-down", lib = "font-awesome")
      #shiny::tags$span(class = "toggle_panel_line")
    )
  )
}

#' Function that generates the variant classification
#' @description given the Consequence and the Variant_Type the function returns the Variant_Classification
#' @param consequence the vector containing the Consequence
#' @param var_type the vector containing the Variant_Type
#' @return the Variant_Classification vector
var_class_gen <- function(consequence, var_type){
  Splice_Site <- c("splice_site", "splice_acceptor_variant", "splice_polypyrimidine_tract_variant", "splice_donor_variant", "transcript_ablation")
  In_Frame_Ins <- c("inframe_insertion", "in_frame_ins")
  Nonsense_Mutation <- c("stop_gained", "nonsense_mutation")
  Nonstop_Mutation <- c("nonstop_mutation", "stop_lost")
  Frame_Shift_Del <- c("frameshift_variant", "frame_shift_del")
  IGR <- c("tf_binding_site_variant", "regulatory_region_variant", "regulatory_region", "intergenic_region", "intergenic_variant", "igr")
  Splice_Region <- c("splice_region_variant", "protein_altering_variant", "splice_region" ,"splice_donor_region_variant")
  In_Frame_Del <- c("in_frame_del", "inframe_deletion")
  tre_prime_UTR <- c("3_prime_utr", "3'utr", "3_prime_utr_variant")
  tre_prime_flank <- c("downstream_gene_variant", "3'flank", "3_prime_flank_variant")
  five_prime_UTR <- c("5_prime_utr", "5_prime_utr_premature_start_codon_gain_variant", "5'utr", "5_prime_utr_variant")
  five_prime_flank <- c("upstream_gene_variant", "5'flank", "5_prime_flank_variant")
  Frame_Shift_Ins <- c("frame_shift_ins", "frameshift_variant")
  Translation_Start_Site <- c("translation_start_site", "initiator_codon_variant", "start_lost")
  Missense_Mutation <- c("coding_sequence_variant", "conservative_missense_variant", "rare_amino_acid_variant", "missense_mutation", "missense_variant")
  RNA <- c("rna","mature_mirna_variant", "non_coding_exon_variant", "non_coding_transcript_exon_variant", "non_coding_transcript_variant", "nc_transcript_variant")
  Silent <- c("incomplete_terminal_codon_variant", "stop_retained_variant", "nmd_transcript_variant", "silent", "synonymous_variant")
  Targeted_Region <- c("","targeted_region")
  Intron <- c("transcript_amplification", "intragenic", "intragenic_variant", "intron_variant", "splice_donor_5th_base_variant", "intron")
  lout <- c()
  for(i in 1:length(consequence)){
    final_val <- ""
    value <- consequence[[i]]

    #gets only the starting consequence
    if (grepl("&", consequence[[i]], fixed = T)){
      value <- unlist(strsplit(consequence[[i]], "&"))[[1]]
    }
    else if (grepl(",", consequence[[i]], fixed = T)){
      value <- unlist(strsplit(consequence[[i]], ","))[[1]]
    }
    value <- tolower(value)

    if (value %in% Splice_Site){final_val <- "Splice_Site"}
    else if (value %in% In_Frame_Ins ){final_val <- "In_Frame_Ins"}
    else if (value %in% Nonsense_Mutation){final_val <- "Nonsense_Mutation"}
    else if (value %in% Frame_Shift_Del){
      if(!is.null(var_type) && var_type[[i]] == "DEL"){
        final_val <- "Frame_Shift_Del"
      }
      if (!is.null(var_type) && value == "frameshift_variant" && var_type[[i]] == "INS"){
        final_val <- "Frame_Shift_Ins"
      }
    }
    else if (value %in% Translation_Start_Site){final_val <- "Translation_Start_Site"}
    else if (value %in% IGR){final_val <- "IGR"}
    else if (value %in% RNA){final_val <- "RNA"}
    else if (value %in% Splice_Region){final_val <- "Splice_Region"}
    else if (value %in% Nonstop_Mutation){final_val <- "Nonstop_Mutation"}
    else if (value %in% In_Frame_Del){final_val <- "In_Frame_Del"}
    else if (value %in% tre_prime_UTR){final_val <- "3'UTR"}
    else if (value %in% tre_prime_flank){final_val <- "3'Flank"}
    else if (value %in% five_prime_UTR){final_val <- "5'UTR"}
    else if (value %in% five_prime_flank){final_val <- "5'Flank"}
    else if (value %in% Frame_Shift_Ins){
      if(!is.null(var_type) && var_type[[i]] == "INS"){
        final_val <- "Frame_Shift_Ins"
      }
      if (!is.null(var_type) && value == "frameshift_variant" && var_type[[i]] == "DEL"){
        final_val <- "Frame_Shift_Del"
      }
    }
    else if (value %in% Missense_Mutation){final_val <- "Missense_Mutation"}
    else if (value %in% Silent){final_val <- "Silent"}
    else if (value %in% Targeted_Region){final_val <- "Targeted_Region"}
    else if (value %in% Intron){final_val <- "Intron"}
    lout <- append(lout, final_val)
  }
  return(lout)
}

#' Function that generates the Variant_Type
#' @description given the VARIANT_CLASS, the Ref and Alt the function returns the Variant_Type
#' @param var_class the vector containing the VARIANT_CLASS
#' @param ref the vector containing the Ref
#' @param alt the vector containing the Alt
#' @return the Variant_Type vector
var_type_gen <- function(var_class, ref, alt){
  lout <- c()
  for(i in 1:length(var_class)){
    if(var_class[[i]] == "SNV"){
      lout <- append(lout, "SNP")
    }
    else if(var_class[[i]] == "substitution"){
      if (!is.null(alt)){
        if(nchar(alt[[i]]) == 1){lout <- append(lout, "SNP")}
        else if(nchar(alt[[i]]) == 2){lout <- append(lout, "DNP")}
        else if(nchar(alt[[i]]) == 3){lout <- append(lout, "TNP")}
        else{lout <- append(lout, "ONP")}
      }
      else{
        lout <- append(lout, "ONP")
      }
    }
    else if(var_class[[i]] == "insertion"){
      lout <- append(lout, "INS")
    }
    else if(var_class[[i]] == "indel"){
      if (!is.null(alt) && !is.null(ref)){
        if(nchar(alt[[i]]) >  nchar(ref[[i]])){lout <- append(lout, "INS")}
        else{lout <- append(lout, "DEL")}
      }
      else{
        lout <- append(lout, "DEL")
      }
    }
    else if (var_class[[i]] %in% c("deletion", "copy_number_variation", "", " ")){
      lout <- append(lout, "DEL")
    }
    else{
      lout <- append(lout, "")
    }
  }
  return(lout)
}


#' Create a tooltip
#' @description
#' used to create a tooltip in the ui
#' @param txt the text to display in the tooltip popup
#' @return the tooltip
#' @examples a_tooltip("text to show")
a_tooltip <- function(txt){
  htmltools::HTML('
    <span data-toggle = "tooltip" class = "tooltip" style = "float: right" data-placement = "right"
    title = ""
    data-original-title = "A tooltip"><i class = "far fa-circle-question" role = "presentation"
    aria-label = "circle-question icon"></i></span>
  ')
}

