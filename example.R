library(hash)

#set working directory to this repo
setwd("~/repos/crossword/")

#read in functions
source("R/functions.R")

#randomly generate xword entries
n_words <- 50
n_letters <- table(sample(5:10, n_words, replace = T))
possible_words <- words::words
words <- unlist(sapply(as.integer(names(n_letters)), function(nl) sample(possible_words$word[possible_words$word_length == nl], 
                                               n_letters[as.character(nl)], replace = F)))

#or read in your own
words_and_clues <- readLines("input/text/2023-Dec_Xmas-Card-Xword.txt", warn = F)
words_and_clues <- readLines("input/text/2023-Dec_Xmas-Card-Xword_Russian.txt", warn = F, encoding = "UTF-8")

#process clues to appropriate format
words_and_clues <- words_and_clues[grepl(":", words_and_clues)]
words_and_clues <- data.frame(trimws(do.call(rbind, strsplit(words_and_clues, split = ":"))))
colnames(words_and_clues) <- c("clues", "words")
words <- tolower(gsub(" ", "", words_and_clues$words))
n_words <- length(words)
clues <- setNames(words_and_clues$clues, words)
full_words <- setNames(words_and_clues$words, words)

#get some useful variables
total_nchar <- nchar(paste0(words, collapse = ""))
n_words <- length(words)
xword <- matrix("", ncol = ceiling(max(sqrt(total_nchar * 5), nchar(words))) * 2, 
                nrow = ceiling(max(sqrt(total_nchar * 5), nchar(words))) * 2)
used <- setNames(rep(F, n_words), words)
word_locs <- lapply(setNames(words, words), function(x) return(NA))

#load in a picture whose shape we want to target, and find the black pixels
pic <- png::readPNG("input/img/black_heart.png")
has_transparency <- min((c(1,0) - mean(pic[,,4] > 0.5)) * c(1,-1)) > c(0.05)
if(has_transparency){
  pic_mat <- pic[,,4] > 0.01
  pic_locs <- which(pic_mat, arr.ind = T)
} else {
  pic_mat <- (pic[,,1] + pic[,,2] + pic[,,3]) <= 1
  pic_locs <- which(pic_mat, arr.ind = T)
}

#### run MCMC ####

#set MCMC params
n_iter <- 100

# generate and draw an initial xword
raw_xword <- generate_xword(words, max_attempts_word_placement = 20)
xword <- raw_xword[["xword"]]
word_locs <- raw_xword[["word_locs"]]
curr_score <- score_xword(xword, words, word_locs)
trimmed_xword_data <- trim_matrix(xword, word_locs)

#specify font properties
ptsize_x <- 5
font_family_for_nums = "Home Christmas"
# font_family_for_nums = "Arial Unicode MS"
png("output/img/init_xword.png", family = font_family_for_nums, width = ncol(trimmed_xword_data$xword) * 30 * ptsize_x, 
    height = nrow(trimmed_xword_data$xword) * 30 * ptsize_x, pointsize = 12 * ptsize_x)
par(xpd = NA, mar = c(0,0,0,0))
draw_xword(trimmed_xword_data$xword, trimmed_xword_data$word_locs, border_lwd = 2 * ptsize_x,
           family = font_family_for_nums, num_cex = 1)
dev.off()


#run MCMC
for(i in 1:n_iter){
  cat(paste0("(i: ", i, ", "))
  prop_raw_xword <- swap_word(xword, words, word_locs)
  prop_score <- score_xword(prop_raw_xword[["xword"]], words, prop_raw_xword[["word_locs"]])
  # prop_score <- prop_score +
  #   ifelse(sum(find_n_extra(prop_raw_xword[["xword"]], words)) == 0,
  #      0,
  #      Inf)
  accept_prob <- invlogit(curr_score - prop_score)
  cat(paste0("\ndNC", ": ", sum(prop_raw_xword$xword != "") - sum(xword != ""),", "))
  if(rbinom(1, 1, prob = accept_prob) == 1){
    
    xword <- prop_raw_xword[["xword"]]
    word_locs <- prop_raw_xword[["word_locs"]]
    curr_score <- prop_score
    cat(paste0("dS", ": ", curr_score))
    
    #draw if desired
    trimmed_xword <- trim_matrix(xword, word_locs)$xword
    trimmed_word_locs <- trim_matrix(xword, word_locs)$word_locs
    png("~/xword.png", family = font_family_for_nums, width = ncol(trimmed_xword_data$xword) * 30 * ptsize_x, 
        height = nrow(trimmed_xword_data$xword) * 30 * ptsize_x, pointsize = 12 * ptsize_x)
    par(xpd = NA, mar = c(0,0,0,0))
    draw_xword(trimmed_xword, trimmed_word_locs, border_lwd = 2 * ptsize_x,
               family = "Arial Unicode MS", num_cex = 1)
    dev.off()
    
  } else {
    cat(paste0("dS", ": ", "^"))
  }
  cat(")\n")

}


#function for running MCMC
run_MCMC <- function(xword, word_locs, n_iter = 100, print_progress = F){

  words <- names(word_locs)
  curr_score <- score_xword(xword, words, word_locs)
  for(i in 1:n_iter){
    if(print_progress){cat(paste0("(i: ", i, ", "))}
    
    prop_raw_xword <- swap_word(xword, words, word_locs)
    prop_score <- score_xword(prop_raw_xword[["xword"]], words, prop_raw_xword[["word_locs"]])
    accept_prob <- 1 - invlogit(curr_score - prop_score)
    
    if(print_progress){cat(paste0("\ndNC", ": ", sum(prop_raw_xword$xword != "") - sum(xword != ""),", "))}
    
    #accept-reject
    if(rbinom(1, 1, prob = accept_prob) == 1){
      
      xword <- prop_raw_xword[["xword"]]
      word_locs <- prop_raw_xword[["word_locs"]]
      curr_score <- prop_score
      if(print_progress){cat(paste0("dS", ": ", curr_score))}
    
    } else {
      if(print_progress){cat(paste0("dS", ": ", "^"))}
    }
    if(print_progress){cat(")\n")}
  }
  
  return(list(xword = xword, word_locs = word_locs))
}

#generate random xwords
xword_dir <- "~/xwords/russian_runs/"
if(!dir.exists(xword_dir)) dir.create(xword_dir)
ptsize_x <- 5
xword_indices <- 101:110
for(i in xword_indices){
  print(i)
  
  raw_xword <- generate_xword(words, max_attempts_word_placement = 20)
  
  #save original and also refined version (MCMC output)
  for(j in 1:2){
    if(j == 2){
      refined_xword <- run_MCMC(raw_xword[["xword"]], raw_xword[["word_locs"]], n_iter = 50)
    } else {
      refined_xword <- raw_xword
    }
    
    save(refined_xword, file = paste0(xword_dir, "xword_", i, ifelse(j==1, "-orig", ""), ".RData"))
    trimmed_xword_data <- trim_matrix(refined_xword[["xword"]], refined_xword[["word_locs"]])
    
    #draw xword with words filled in
    png(paste0(xword_dir, "xword_", i, ifelse(j==1, "-orig", ""), ".png"), family = font_family_for_nums, width = ncol(trimmed_xword_data$xword) * 30 * ptsize_x, 
        height = nrow(trimmed_xword_data$xword) * 30 * ptsize_x, pointsize = 12 * ptsize_x)
    par(xpd = NA, mar = c(0,0,0,0))
    draw_xword(trimmed_xword_data$xword, trimmed_xword_data$word_locs, border_lwd = 2 * ptsize_x,
               family = "Arial Unicode MS", num_cex = 1)
    dev.off()
    
    #draw blank xword
    png(paste0(xword_dir, "xword_", i, ifelse(j==1, "-orig", ""), "_blank.png"), family = font_family_for_nums, width = ncol(trimmed_xword_data$xword) * 30 * ptsize_x, 
        height = nrow(trimmed_xword_data$xword) * 30 * ptsize_x, pointsize = 12 * ptsize_x)
    par(xpd = NA, mar = c(0,0,0,0))
    draw_xword(trimmed_xword_data$xword, trimmed_xword_data$word_locs, border_lwd = 2 * ptsize_x, write_words = F,
               family = font_family_for_nums, num_cex = 1)
    dev.off()
    
    #draw shape of xword
    png(paste0(xword_dir, "xword_", i, ifelse(j==1, "-orig", ""), "_shape.png"), family = font_family_for_nums, width = ncol(trimmed_xword_data$xword) * 30 * ptsize_x, 
        height = nrow(trimmed_xword_data$xword) * 30 * ptsize_x, pointsize = 12 * ptsize_x)
    par(xpd = NA, mar = c(0,0,0,0))
    draw_xword(trimmed_xword_data$xword, trimmed_xword_data$word_locs, border_lwd = 2 * ptsize_x, write_words = F, 
               cell_color = "black", 
               background_color = "white",
               family = font_family_for_nums, num_cex = 1, drop_shadow = F, cell_scale = 1)
    dev.off()
    
    #save words and clues
    word_numbers <- number_words(trimmed_xword_data$word_locs)
    
    sink(paste0(xword_dir, "xword_", i, ifelse(j==1, "-orig", ""), "_answers-clues.txt"))
    
    cat("ANSWERS:\n\n")
    cat(paste0("\nHorizontal: ", paste0(word_numbers$word_order$h$num, ". ", full_words[word_numbers$word_order$h$word], collapse = " ")))
    cat(paste0("\n\nVertical: ", paste0(word_numbers$word_order$v$num, ". ", full_words[word_numbers$word_order$v$word], collapse = " ")))
    
    cat("\n\nCLUES:\n\n")
    cat(paste0("\nHorizontal: ", paste0(word_numbers$word_order$h$num, ". ", clues[word_numbers$word_order$h$word], collapse = " ")))
    cat(paste0("\n\nVertical: ", paste0(word_numbers$word_order$v$num, ". ", clues[word_numbers$word_order$v$word], collapse = " ")))
    
    sink()
    
  }
  
}

#### further refinement ####
# iterate over favorites for further refinements
xword_dir <- "~/xwords/fave-iterations/"
if(!dir.exists(xword_dir)) dir.create(xword_dir)
xword_indices <- 1:50
fave_i <- 244
ptsize_x <- 5
for(i in xword_indices){
  
  load(paste0("~/xwords/runs/xword_", fave_i, ".RData"))
  
  if(i != 1){
    refined_xword <- run_MCMC(refined_xword[["xword"]], refined_xword[["word_locs"]], n_iter = 10)  
  }
  save(refined_xword, file = paste0(xword_dir, "xword_", fave_i, "-", i, ".RData"))
  trimmed_xword_data <- trim_matrix(refined_xword[["xword"]], refined_xword[["word_locs"]])
  
  #draw xword with words filled in
  png(paste0(xword_dir, "xword_", fave_i, "-", i, ".png"), family = font_family_for_nums, width = ncol(trimmed_xword_data$xword) * 30 * ptsize_x, 
      height = nrow(trimmed_xword_data$xword) * 30 * ptsize_x, pointsize = 12 * ptsize_x)
  par(xpd = NA, mar = c(0,0,0,0))
  draw_xword(trimmed_xword_data$xword, trimmed_xword_data$word_locs, border_lwd = 2 * ptsize_x,
             family = font_family_for_nums, num_cex = 1)
  dev.off()
  
  #draw blank xword
  png(paste0(xword_dir, "xword_", fave_i, "-", i, "_blank.png"), family = font_family_for_nums, width = ncol(trimmed_xword_data$xword) * 30 * ptsize_x, 
      height = nrow(trimmed_xword_data$xword) * 30 * ptsize_x, pointsize = 12 * ptsize_x)
  par(xpd = NA, mar = c(0,0,0,0))
  draw_xword(trimmed_xword_data$xword, trimmed_xword_data$word_locs, border_lwd = 2 * ptsize_x, write_words = F,
             family = font_family_for_nums, num_cex = 1)
  dev.off()
  
  #draw shape of xword
  png(paste0(xword_dir, "xword_", fave_i, "-", i, "_shape.png"), family = font_family_for_nums, width = ncol(trimmed_xword_data$xword) * 30 * ptsize_x, 
      height = nrow(trimmed_xword_data$xword) * 30 * ptsize_x, pointsize = 12 * ptsize_x)
  par(xpd = NA, mar = c(0,0,0,0))
  draw_xword(trimmed_xword_data$xword, trimmed_xword_data$word_locs, border_lwd = 2 * ptsize_x, write_words = F, 
             cell_color = "black", 
             background_color = "white",
             family = font_family_for_nums, num_cex = 1)
  dev.off()
  
  #save words and clues
  word_numbers <- number_words(trimmed_xword_data$word_locs)
  
  sink(paste0(xword_dir, "xword_", fave_i, "-", i, "_answers-clues.txt"))
  
  cat("ANSWERS:\n\n")
  cat(paste0("\nHorizontal: ", paste0(word_numbers$word_order$h$num, ". ", full_words[word_numbers$word_order$h$word], collapse = " ")))
  cat(paste0("\n\nVertical: ", paste0(word_numbers$word_order$v$num, ". ", full_words[word_numbers$word_order$v$word], collapse = " ")))
  
  cat("\n\nCLUES:\n\n")
  cat(paste0("\nHorizontal: ", paste0(word_numbers$word_order$h$num, ". ", clues[word_numbers$word_order$h$word], collapse = " ")))
  cat(paste0("\n\nVertical: ", paste0(word_numbers$word_order$v$num, ". ", clues[word_numbers$word_order$v$word], collapse = " ")))
  
  sink()
  
}