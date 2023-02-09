nReturn = inputbox("Please enter your password:", "System login", "*")
if not empty(nReturn)
    select users
    seek username()
    if password = nReturn
        // match
    else
        // nomatch
    endif
    // no password entered
endif