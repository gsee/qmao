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
    else switch(month, jan=, Jan='F',feb=, Feb='G',
                mar=, Mar='H', apr=, Apr='J',
                may=, May='K', jun=, Jun='M',
                jul=, Jul='N', aug=, Aug='Q',
                sep=, Sep='U', oct=, Oct='V',
                nov=, Nov='X', dec=, Dec='Z')
}

