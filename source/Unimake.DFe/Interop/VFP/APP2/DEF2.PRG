**************************************************
*-- Class:        cussqlthermo (c:\vmp2005\my\cussqlthermo.vcx)
*-- ParentClass:  custom
*-- BaseClass:    custom
*-- Time Stamp:   10/18/05 09:07:11 PM
*
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
        LOCAL lcNewOutputWindow, lcOldOutputWindow, lcOldSetNotify, lcOldSetTalk
        lcNewOutputWindow = THIS.icNewOutputWindow
        lcOldOutputWindow = THIS.icOldOutputWindow
        lcOldSetNotify = THIS.icOldSetNotify
        lcOldSetTalk = THIS.icOldSetTalk

        IF VERSION(5)>=800
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

            .icNewOutputWindow = "SQLThermo"+SYS(2015)
            LOCAL m.lcNewOutputWindow
            m.lcNewOutputWindow = .icNewOutputWindow
            DEFINE WINDOW &lcNewOutputWindow. FROM -1000,-1000 TO -500,-500
            ACTIVATE WINDOW &lcNewOutputWindow. IN SCREEN

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
*
*-- EndDefine: cussqlthermo
**************************************************