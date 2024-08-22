eqn_parser <- function(string,
                       `**`="boldsymbol", `*`="mathcal", `%`="mathrm"){

    # `*` -> \times
    if(grepl('`*`', string))
        string <- gsub('`*`', "\\times ", string, fixed=TRUE)

    # %n -> \\\\ \n
    if(grepl('%n', string))
        string <- gsub('%n', "\\\\ \n", string, fixed=TRUE)

    # **.**
    if(grepl('\\*\\*', string)){
        loc <- gregexpr("\\*\\*", string)[[1L]]
        for(i in loc[seq(length(loc), 2, by=-2)])
            substring(string, i, i+1L) <- '}%'
        string <- gsub('%', '', string)
        string <- gsub('\\*\\*', replacement = sprintf("\\\\%s{", `**`), string)
    }

    # *.*
    # Note: this would have a problem with A * B
    if(grepl('\\*', string)){
        loc <- gregexpr("\\*", string)[[1L]]
        for(i in loc[seq(length(loc), 2, by=-2)])
            substring(string, i, i+1L) <- '}'
        string <- gsub('\\*', replacement = sprintf("\\\\%s{", `*`), string)
    }

    # %.%
    if(grepl('\\%', string)){
        loc <- gregexpr("\\%", string)[[1L]]
        for(i in loc[seq(length(loc), 2, by=-2)])
            substring(string, i, i+1L) <- '}'
        string <- gsub('\\%', replacement = sprintf("\\\\%s{", `%`), string)
    }

    string
}

if(FALSE){
    s1 <- "e = mc^2"
    s2 <- "**e** = **mc**^2"
    s3 <- "\\mathcal{H}_0 : \\mathbf{C} \\mathbf{B} & = "
    s4 <- "*H*_0 : *C B* & = "
    s5 <- "*H*_0 : *C* *B* & = "
    s6 <- "*H*_0 : %n *C* *B* & = "
    s7 <- "%H%_0 : %C% %B% = "
    s8 <- "*H*_0 : *C* `*` *B*  = "

    identical(eqn_parser(s1), s1)
    eqn_parser(s2)
    eqn_parser(s2, `**` = 'mathbf')
    identical(eqn_parser(s3), s3)
    eqn_parser(s4)
    eqn_parser(s5)
    eqn_parser(s6)
    eqn_parser(s7)
    eqn_parser(s8)

}
