DEFINE CLASS cussqlthermo AS custom


    *-- Holds SET TALK setting at initialization.
    PROTECTED icoldsettalk
    icoldsettalk = ""
    *-- Holds SET NOTIFY setting at initialization.
    icoldsetnotify = ""
    *-- Holds the output window at initialization.
    PROTECTED icoldoutputwindow
    icoldoutputwindow = ""
    *-- Holds the name of the temporary output window.
    PROTECTED icnewoutputwindow
    icnewoutputwindow = ""
    Name = "cussqlthermo"


    PROCEDURE Destroy
        SET TALK OFF
        LOCAL ;
          m.lcNewOutputWindow, ;
          m.lcOldOutputWindow, ;
          m.lcOldSetNotify, ;
          m.lcOldSetTalk
        WITH THIS
          m.lcNewOutputWindow = .icNewOutputWindow
          m.lcOldOutputWindow = .icOldOutputWindow
          m.lcOldSetNotify = .icOldSetNotify
          m.lcOldSetTalk = .icOldSetTalk
        ENDWITH

        IF VERSION(5)>=700
          SET NOTIFY &lcOldSetNotify.
        ENDIF

        SET TALK &lcOldOutputWindow.

        RELEASE WINDOWS &lcNewOutputWindow.

        SET TALK &lcOldSetTalk.
    ENDPROC


    PROCEDURE Init
        *Class that activates SQL thermometer bar during init
        *and restores altered settings during destroy.

        WITH THIS

            .icOldSetTalk = SET("TALK")
            SET TALK OFF

            .icOldOutputWindow = SET("TALK",1)

            .icNewOutputWindow = "ZZZZZZ"+SYS(2015)
            LOCAL m.lcNewOutputWindow
            m.lcNewOutputWindow = .icNewOutputWindow
         &&   DEFINE WINDOW &lcNewOutputWindow. FROM -1000,-1000 TO -500,-500 TITLE ""
            DEFINE WINDOW "AGUARDE" FROM -1000,-1000 TO -500,-500 TITLE ""
         
            ACTIVATE WINDOW "AGUARDE" IN SCREEN
            SET TALK WINDOW &lcNewOutputWindow.
            IF VERSION(5)>=700
                .icOldSetNotify = SET("NOTIFY")
                SET NOTIFY ON
            ENDIF
        ENDWITH

        SET TALK ON

        RETURN .T.
    ENDPROC


ENDDEFINE




