C2M <- function(code) {
    if (missing(code)) c(F='Jan',G='Feb',H='Mar',
                    J='Apr',K='May',M='Jun',
                    N='Jul',Q='Aug',U='Sep',
                    V='Oct',X='Nov',Z='Dec')    
    else switch(code, F='Jan', G='Feb',H='Mar',
                    J='Apr',K='May',M='Jun',
                    N='Jul',Q='Aug',U='Sep',
                    V='Oct',X='Nov',Z='Dec')
}

M2C <- function(month) {
    if (missing(month)) c(jan='F',feb='G',mar='H',
                    apr='J',may='K',jun='M',
                    jul='N',aug='Q',sep='U',
                    oct='V',nov='X',dec='Z')
    else switch(toupper(month), JAN=, JANUARY='F',
                FEB=, FEBRUARY='G',MAR=, MARCH='H', 
                APR=, APRIL='J', MAY='K', JUN=, JUNE='M',
                JUL=, JULY='N', AUG=, AUGUST='Q',
                SEP=, SEPTEMBER='U', OCT=, OCTOBER='V',
                NOV=, NOVEMBER='X', DEC=, DECEMBER='Z')
}


