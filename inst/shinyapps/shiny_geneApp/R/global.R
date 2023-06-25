write_maf_file <- function(maf, basename = NULL){

  if(is.null(basename)){
    stop('Please provide a basename for output file.')
  }
  #write main maf
  data.table::fwrite(x = data.table::rbindlist(list(maf@data, maf@maf.silent), use.names = TRUE, fill = TRUE),
                     file = basename, sep='\t',
                     quote = FALSE, row.names = FALSE)
}

write_gene_summary <- function(maf, basename = NULL){
  if(is.null(basename)){
    stop('Please provide a basename for output file.')
  }
  #write gene summary.
  write.table(x = maf, file = basename, sep='\t', quote = FALSE, row.names = FALSE)
}

write_sample_summary <- function(maf, basename = NULL){
  if(is.null(basename)){
    stop('Please provide a basename for output file.')
  }
  #write sample summary.
  write.table(x = maf, file = basename, sep='\t', quote = FALSE, row.names = FALSE)
}

write_maf_summary <- function(maf, basename = NULL){
  if(is.null(basename)){
    stop('Please provide a basename for output file.')
  }
  #write summary
  write.table(x = maf,file = basename, sep='\t', quote = FALSE, row.names = FALSE)
}




#' useless tooltip
#' @description
#' function that activates tooltips
#' use once!
#' @examples tooltip_inutile()
tooltip_inutile <- function() {
  shiny::fluidRow(
    shiny::tags$span(
      style = "display:none;",
      `data-toggle` = "tooltip",
      `data-placement` = "left",
      title = "A tooltip",
      shiny::icon("question-circle")
    )
  )
}

#' Check for column names
#' @description
#' check for column names
#' @param n the column name to check
#' @return
#' the correct name or NULL
#' @examples checknames("nomeesempio")
check_names <- function(n) {
  Gene <- c("Gene.refGene","Gene","gene")
  Hugo_Symbol <- c("hugo_symbol" ,"Hugo_Symbol","HUGO_SYMBOL") #"SYMBOL","Symbol","symbol"
  Chromosome <- c("CHROM", "Chromosome","Chr","chrom","chromosome")
  Ref <- c("reference_allele","Reference_Allele","Ref","REF") #,"Tumor_Seq_Allele1"
  Alt <- c("Alt","ALT","Tumor_Seq_Allele2","alt")
  VAF <- c("vaf","VAF","Vaf")
  Variant_Classification <- c("Variant_Classification","Func.refGene")#"Consequence","consequence"
  Variant_Type <- c("Variant_Type","variant_type","ExonicFunc.refGene")
  VARIANT_CLASS <- c("VARIANT_CLASS","variant_class")
  Clinvar <- c("CLIN_SIG","clinvar","Clinvar")
  Depth <- c("t_depth","depth","Depth")
  Start_Position <- c("Start_Position","start","Start")
  End_Position <- c("End_Position","end","End")
  Variation <- c("Existing_Variation","Existing_variation","AAChange.refGene","Variation","Var","variation")
  HGVSp <- c("HGVSp","hgvsp") #"HGVSp_Short",
  Exon <- c("EXON","exon","Exon") #"Exon_Number",
  Tumor_Sample_Barcode <- c("Tumor_Sample_Barcode", "tumor_sample_barcode")
  #VARIE <- c("Actionable.O","Actionable.M","Actionable.C","Moderate.risk","azionabile","High.risk","actionable","Amino_acids","Protein_position","cancervar_tier","tiering","POS","pos")

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
  #else if (n %in% VARIE){NULL}
  else {NULL}
}

check_names_ok <- function(n) {
  if (n == "Gene"){"Gene"}
  else if (n == "Hugo_Symbol"){"HugoSymbol"}
  else if (n == "Chromosome"){"Chromosome"}
  else if (n == "Reference_Allele"){"Ref"}
  else if (n == "Tumor_Seq_Allele2"){"Alt"}
  else if (n == "VAF"){"VAF"}
  else if (n == "Variant_Classification"){"Classification"}
  else if (n == "Variant_Type"){"VariantType"}
  else if (n == "VARIANT_CLASS"){"VariantClass"}
  else if (n == "CLIN_SIG"){"Clinvar"}
  else if (n == "t_depth"){"Depth"}
  else if (n == "Start_Position"){"Start"}
  else if (n == "End_Position"){"End"}
  else if (n == "Existing_Variation"){"Variation"}
  else if (n == "HGVSp"){"HGVSp"}
  else if (n == "EXON"){"Exon"}
  else if (n == "Tumor_Sample_Barcode"){"SampleBarcode"}
  else {NULL}
}

#' check vector type
#' @description
#' this function checks if it is needed
#' to invoke an observer for the matching input
#' @param x the vector to check
#' @return  TRUE if the condition checks NULL otherwise
#' @examples check_ui(dataframecolumn)
check_ui <- function(x) {
  if (is.numeric(x)) {

    rng <- range(x, na.rm = T)
    if (!(rng[1] == rng[2])) {T}
    else {NULL}
  }
  else if (is.factor(x)){ T
  }
  else if ( is.array(x) || is.character(x)) {

    #levs <- levels(factor(x, exclude = NULL))
    #if (length(levs) < 600 && length(levs) > 1){

    levs <- kit::funique(x)
    #remember that this condition is tied to make_ui/2
    #if (length(levs) < (length(x)/8)){T}
    #else {NULL}
    T
  }
  else if (is.logical(x)) {
    if(all(is.na(x)) || all(is.null(x))) {NULL}
    else {T}
  }
  else {
    # Not supported
    NULL
  }
}

#' make UI of somatic file
#' @description
#' this function creates the inputs for the supplied vectors
#' @param x the vector
#' @param var the name of the vector
#' @param id the module's id where the inputs will be created
#' @return the filter created according to the conditions in the function
#' @examples make_ui(vector,name,moduleid)
make_ui <- function(x, var, id, n, s) {
  var2 <- paste(var,n,sep="")
  #NUMERIC VECTORS
  if (is.numeric(x)) {
    rng <- range(x, na.rm = T)
    if (!(rng[1] == rng[2])){
    shiny::sliderInput(paste(s,shiny::NS(id,var2),sep=""), var, min = rng[1], max = rng[2], value = rng)
    }
    else {NULL}
  }
  #FACTORS
  else if (is.factor(x)) {
    levs <- levels(factor(x, exclude = NULL))
    shinyWidgets::pickerInput(paste(s,shiny::NS(id,var2),sep=""), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox= TRUE, size = 10),choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40)) )
  }
  #CARACTER VECTORS AND ARRAYS
  else if ( is.array(x) || is.character(x)) {
    ### --- TEST VARIOUS INPUTS HERE --- ###
    levs <- kit::funique(x)

    if (NA %in% levs){
      levs <- levs[!is.na(levs)]
      levs <- append(levs,"NA")
    }
    #per un select normalepuoi fare il truncate e fare un vettore con c(nomi,valori) per le select
    #FILTERING OF VECTORS
    #if (length(levs) < (length(x)/8)){
    #if (length(levs) < 600 && length(levs) > 1){
      shinyWidgets::pickerInput(paste(s,shiny::NS(id,var2),sep=""), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox= TRUE, size = 10, virtualScroll= TRUE ),choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40)) )
      #shiny::selectizeInput(var, var, choices = levs, selected = levs, multiple = TRUE)
    #}
   # else {NULL}
    #shiny::selectInput(var, var, choices = levs, selected = levs, multiple = TRUE, selectize = T)
  }
  #LOGICAL VECTORS
  else if (is.logical(x)) {
    if(all(is.na(x)) || all(is.null(x))) {NULL}
    else{
      levs <- levels(factor(x, exclude = NULL))
      shinyWidgets::pickerInput(paste(s,shiny::NS(id,var2),sep=""), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox= TRUE, size = 10),choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40)))
    }
  }
  else {
    # Not supported
    NULL
  }
}

#' make UI of germ file
#' DEPRECATED
#' @description
#' this function creates the inputs for the supplied vectors
#' @param x the vector
#' @param var the name of the vector
#' @param id the module's id where the inputs will be created
#' @return the filter created according to the conditions in the function
#' @examples make_ui2(vector,name,moduleid)
make_ui2 <- function(y, var, id){
  #to distinguish from the first
  var2 <- paste(var,"2",sep="")
  x <- y
  if(is.list(y)){x <- as.vector(y)}
  if (is.numeric(x)) {
    rng <- range(x, na.rm = T)
    shiny::sliderInput(shiny::NS(id,var2), var, min = rng[1], max = rng[2], value = rng)
  }
  else if (is.factor(x)) {
    levs <- levels(factor(x, exclude = NULL))
    shinyWidgets::pickerInput(shiny::NS(id,var2), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox= TRUE, size = 10),choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40))  )
  }
  else if ( is.array(x) || is.character(x)) {

    #levs <- levels(factor(x, exclude = NULL))

    levs <- kit::funique(x)
    #if (length(levs) < (length(x)/8)){
      shinyWidgets::pickerInput(shiny::NS(id,var2), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox= TRUE, size = 10),choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40))  )
    #}
    #else {NULL}
  }
  else if (is.logical(x)) {
    if(all(is.na(x)) || all(is.null(x))) {NULL}
    else{
      levs <- levels(factor(x, exclude = NULL))
      shinyWidgets::pickerInput(shiny::NS(id,var2), var, choices = levs, selected = levs, multiple = TRUE, options = shinyWidgets::pickerOptions(actionsBox= TRUE, size = 10),choicesOpt = list(content = stringr::str_trunc(c(levs), width = 40)))
    }
  }
  else {
    # Not supported
    NULL
  }
}


#' Filter function
#' @description
#' this function decide if the values in the table are to keep or not
#' @param x the vector to filter
#' @param val the values supplied from the inputs
#' @return a logical vector mapping the values to remove from the tableù
#' @examples filter_var(vector,inputvalues)
filter_var <- function(x, val) {
  if (is.numeric(x) ) {
    if (val[1]==val[2]) {TRUE}
    else {
      !is.na(x) & x >= val[1] & x <= val[2]
    }
  }
  else if (is.factor(x)){

    x %in% val
  }
  else if (is.character(x)) {
    #v <- levels(factor(x, exclude = NULL))
    l <- c()
    for( c in x){
      if(c %in% val){l <- append(l,T)}
      else if (is.na(c) && "NA" %in% val){l <- append(l,T)}
      else{l <- append(l,F)}
    }
    l
    #x %in% val
  }
  else if (is.logical(x)) {T}
  else {
    # No control, so don't filter
    TRUE
  }
}
#' Filter function
#' @description
#' this function decide if the values in the table are to keep or not
#' @param x the vector to filter
#' @param val the values supplied from the inputs
#' @return a logical vector mapping the values to remove from the tableù
#' @examples filter_var(vector,inputvalues)
filter_var_multi <- function(x, val) {
  if (is.numeric(x) ) {
    if (val[1]==val[2]) {TRUE}
    else {
      is.na(x) | (x >= val[1] & x <= val[2])
    }
  }
  else if (is.factor(x)){

    x %in% val
  }
  else if (is.character(x)) {
    #v <- levels(factor(x, exclude = NULL))
    l <- c()
    for( c in x){
      if(c %in% val){l <- append(l,T)}
      else if (is.na(c) && "NA" %in% val){l <- append(l,T)}
      else{l <- append(l,F)}
    }
    l
    #x %in% val
  }
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
#' @examples toggle_panel(toggleid,targetid,text)
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
      shiny::tags$span(class = "toggle_panel_line")
    )
  )
}

consq_gen <- function(cq,vt){
  Splice_Site <- c("Splice_Site","splice_site","splice_acceptor_variant","splice_polypyrimidine_tract_variant","splice_donor_variant","transcript_ablation")
  In_Frame_Ins <- c("inframe_insertion","In_Frame_Ins","in_frame_ins")
  Nonsense_Mutation <- c("stop_gained","Stop_gained","Nonsense_Mutation")
  Nonstop_Mutation <- c("Nonstop_Mutation","nonstop_mutation","stop_lost")
  Frame_Shift_Del <- c("frameshift_variant","Frame_Shift_Del","frame_shift_del")
  IGR <- c("TF_binding_site_variant","regulatory_region_variant","regulatory_region","intergenic_region","intergenic_variant","IGR","igr")
  Splice_Region <- c("splice_region_variant","protein_altering_variant","splice_region","Splice_Region","splice_donor_region_variant")
  In_Frame_Del <- c("in_frame_del","In_Frame_Del","inframe_deletion")
  tre_prime_UTR <- c("3_prime_UTR","3'UTR","3_prime_UTR_variant")
  tre_prime_flank <- c("downstream_gene_variant","3'Flank", "3_prime_Flank_variant")
  five_prime_UTR <- c("5_prime_UTR","5_prime_UTR_premature_start_codon_gain_variant","5'UTR","5_prime_UTR_variant")
  five_prime_flank <- c("upstream_gene_variant","5'Flank","5_prime_Flank_variant")
  Frame_Shift_Ins <- c("Frame_Shift_Ins","frame_shift_ins","frameshift_variant") ##ce ne sono due con frame shift variant
  Translation_Start_Site <- c("Translation_Start_Site","translation_start_site","initiator_codon_variant","start_lost")
  Missense_Mutation <- c("coding_sequence_variant","conservative_missense_variant","rare_amino_acid_variant","Missense_Mutation","missense_mutation","missense_variant")
  RNA <- c("RNA","rna","mature_miRNA_variant","non_coding_exon_variant","non_coding_transcript_exon_variant","non_coding_transcript_variant","nc_transcript_variant")
  Silent <- c("incomplete_terminal_codon_variant","stop_retained_variant","NMD_transcript_variant","Silent","silent","synonymous_variant")
  Targeted_Region <- c("","Targeted_Region","targeted_region")
  Intron <- c("transcript_amplification","INTRAGENIC","intragenic_variant","Intron","intron_variant","splice_donor_5th_base_variant","intron")
  #"splice_region_variant" "protein_altering_variant" "splice_region_variant&splice_polypyrimidine_tract_variant&intron_variant" "splice_acceptor_variant&splice_polypyrimidine_tract_variant&coding_sequence_variant&intron_variant"
  lout <- c()
  for(i in 1:length(cq)){
    x <- ""
    cx <- cq[[i]]
    if (grepl("&",cq[[i]],fixed=T)){
      cx <- unlist(strsplit(cq[[i]],"&"))[[1]]
    }
    else if (grepl(",",cq[[i]],fixed=T)){
      cx <- unlist(strsplit(cq[[i]],","))[[1]]
    }
    if (cx %in% Splice_Site){x <- "Splice_Site"}
    else if (cx %in% In_Frame_Ins ){x <- "In_Frame_Ins"}
    else if (cx %in% Nonsense_Mutation){x <- "Nonsense_Mutation"}
    else if (cx %in% Frame_Shift_Del){
      if(!is.null(vt) && vt[[i]] == "DEL"){
        x <- "Frame_Shift_Del"
      }
      if (!is.null(vt) && cx == "frameshift_variant" && vt[[i]] == "INS"){x <- "Frame_Shift_Ins"}
    }
    else if (cx %in% Translation_Start_Site){x <- "Translation_Start_Site"}
    else if (cx %in% IGR){x <- "IGR"}
    else if (cx %in% RNA){x <- "RNA"}
    else if (cx %in% Splice_Region){x <- "Splice_Region"}
    else if (cx %in% Nonstop_Mutation){x <- "Nonstop_Mutation"}
    else if (cx %in% In_Frame_Del){x <- "In_Frame_Del"}
    else if (cx %in% tre_prime_UTR){x <- "3'UTR"}
    else if (cx %in% tre_prime_flank){x <- "3'Flank"}
    else if (cx %in% five_prime_UTR){x <- "5'UTR"}
    else if (cx %in% five_prime_flank){x <- "5'Flank"}
    else if (cx %in% Frame_Shift_Ins){
      if(!is.null(vt) && vt[[i]] == "INS"){
        x <- "Frame_Shift_Ins"
      }
      if (!is.null(vt) && cx == "frameshift_variant" && vt[[i]] == "DEL"){x <- "Frame_Shift_Del"}
    }
    else if (cx %in% Missense_Mutation){x <- "Missense_Mutation"}
    else if (cx %in% Silent){x <- "Silent"}
    else if (cx %in% Targeted_Region){x <- "Targeted_Region"}
    else if (cx %in% Intron){x <- "Intron"}
    lout<-append(lout,x)
  }
  return(lout)
}

vc_gen <- function(vc,ref,alt){
  lout <- c()
  for(i in 1:length(vc)){
    if(vc[[i]] == "SNV"){
      lout <- append(lout,"SNP")
    }
    else if(vc[[i]] == "substitution"){
      if (!is.null(alt)){
        if(nchar(alt[[i]]) == 1){lout <- append(lout,"SNP")}
        else if(nchar(alt[[i]]) == 2){lout <- append(lout,"DNP")}
        else if(nchar(alt[[i]]) == 3){lout <- append(lout,"TNP")}
        else{lout <- append(lout,"ONP")}
      }
      else{
        lout <- append(lout,"ONP")
      }
    }
    else if(vc[[i]] == "insertion"){
      lout <- append(lout,"INS")
    }
    else if(vc[[i]] == "indel"){
      if (!is.null(alt) && !is.null(ref)){
        if(nchar(alt[[i]]) >  nchar(ref[[i]])){lout <- append(lout,"INS")}
        else{lout <- append(lout,"DEL")}
      }
      else{
        lout <- append(lout,"DEL")
      }
    }
    else if (vc[[i]] %in% c("deletion","copy_number_variation",""," ")){ #?
      lout <- append(lout,"DEL")
    }
    else{
      lout <- append(lout,"")
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
  shiny::fluidRow(
    shiny::tags$span(
      style="float:right; margin-right:5%; margin-top:2%;",
      `data-toggle` = "tooltip",
      `data-placement` = "left",
      title = txt,
      shiny::icon("question-circle")
    )
  )
}

