LOCAL loOauth2
LOCAL lnSuccess
LOCAL lcUrl
LOCAL lnNumMsWaited
LOCAL loJson
LOCAL loFac

* This example requires the Chilkat API to have been previously unlocked.
* See Global Unlock Sample for sample code.

* See more Microsoft Graph OAuth2 examples:
* 
* Microsoft Graph OAuth2 App Authentication using Azure AD
* Microsoft Graph OAuth2 Client Credentials Grant Flow
* Microsoft Graph -- Renew Expiring Access Token (Azure AD v2.0 Endpoint)
* Microsoft Graph -- Renew Expiring Access Token (Azure AD Endpoint)
* Decode Microsoft Graph ID Token
* Microsoft Graph Revoke OAuth2 Access Tokens

loOauth2 = CreateObject('Chilkat_9_5_0.OAuth2')

* This should be the port in the localhost callback URL for your app.  
* The callback URL would look like "http://localhost:3017/" if the port number is 3017.
loOauth2.ListenPort = 3017

loOauth2.AuthorizationEndpoint = "https://login.microsoftonline.com/common/oauth2/v2.0/authorize"
loOauth2.TokenEndpoint = "https://login.microsoftonline.com/common/oauth2/v2.0/token"

* Replace these with actual values.
loOauth2.ClientId = "MICROSOFT-GRAPH-CLIENT-ID"
* This is your app password:
loOauth2.ClientSecret = "MICROSOFT-GRAPH-CLIENT-SECRET"

loOauth2.CodeChallenge = 0
* Provide a SPACE separated list of scopes.
* See https://developer.microsoft.com/en-us/graph/docs/authorization/permission_scopes 

* Important: To get a refresh token in the final response, you have to ask for "offline_access" scope
loOauth2.Scope = "openid profile offline_access user.readwrite calendars.readwrite files.readwrite"

* Begin the OAuth2 three-legged flow.  This returns a URL that should be loaded in a browser.
lcUrl = loOauth2.StartAuth()
IF (loOauth2.LastMethodSuccess <> 1) THEN
    ? loOauth2.LastErrorText
    RELEASE loOauth2
    CANCEL
ENDIF

* At this point, your application should load the URL in a browser.
* For example, 
* in C#: System.Diagnostics.Process.Start(url);
* in Java: Desktop.getDesktop().browse(new URI(url));
* in VBScript: Set wsh=WScript.CreateObject("WScript.Shell")
*              wsh.Run url
* in Xojo: ShowURL(url)  (see http://docs.xojo.com/index.php/ShowURL)
* in Dataflex: Runprogram Background "c:\Program Files\Internet Explorer\iexplore.exe" sUrl        
* The Microsoft account owner would interactively accept or deny the authorization request.

* Add the code to load the url in a web browser here...
* Add the code to load the url in a web browser here...
* Add the code to load the url in a web browser here...

* Now wait for the authorization.
* We'll wait for a max of 30 seconds.
lnNumMsWaited = 0
DO WHILE (lnNumMsWaited < 30000) AND (loOauth2.AuthFlowState < 3)
    loOauth2.SleepMs(100)
    lnNumMsWaited = lnNumMsWaited + 100
ENDDO

* If there was no response from the browser within 30 seconds, then 
* the AuthFlowState will be equal to 1 or 2.
* 1: Waiting for Redirect. The OAuth2 background thread is waiting to receive the redirect HTTP request from the browser.
* 2: Waiting for Final Response. The OAuth2 background thread is waiting for the final access token response.
* In that case, cancel the background task started in the call to StartAuth.
IF (loOauth2.AuthFlowState < 3) THEN
    loOauth2.Cancel()
    ? "No response from the browser!"
    RELEASE loOauth2
    CANCEL
ENDIF

* Check the AuthFlowState to see if authorization was granted, denied, or if some error occurred
* The possible AuthFlowState values are:
* 3: Completed with Success. The OAuth2 flow has completed, the background thread exited, and the successful JSON response is available in AccessTokenResponse property.
* 4: Completed with Access Denied. The OAuth2 flow has completed, the background thread exited, and the error JSON is available in AccessTokenResponse property.
* 5: Failed Prior to Completion. The OAuth2 flow failed to complete, the background thread exited, and the error information is available in the FailureInfo property.
IF (loOauth2.AuthFlowState = 5) THEN
    ? "OAuth2 failed to complete."
    ? loOauth2.FailureInfo
    RELEASE loOauth2
    CANCEL
ENDIF

IF (loOauth2.AuthFlowState = 4) THEN
    ? "OAuth2 authorization was denied."
    ? loOauth2.AccessTokenResponse
    RELEASE loOauth2
    CANCEL
ENDIF

IF (loOauth2.AuthFlowState <> 3) THEN
    ? "Unexpected AuthFlowState:" + STR(loOauth2.AuthFlowState)
    RELEASE loOauth2
    CANCEL
ENDIF

? "OAuth2 authorization granted!"
? "Access Token = " + loOauth2.AccessToken

* Get the full JSON response:
loJson = CreateObject('Chilkat_9_5_0.JsonObject')
loJson.Load(loOauth2.AccessTokenResponse)
loJson.EmitCompact = 0

* The JSON response looks like this:

* {
*   "token_type": "Bearer",
*   "scope": "openid profile User.ReadWrite Calendars.ReadWrite Files.ReadWrite ...",
*   "expires_in": 3600,
*   "ext_expires_in": 3600,
*   "access_token": "EwBAA8l6B...",
*   "refresh_token": "MCRMdbe...",
*   "id_token": "eyJ0eXA..."
* }

? loJson.Emit()

* Save the JSON to a file for future requests.
loFac = CreateObject('Chilkat_9_5_0.FileAccess')
loFac.WriteEntireTextFile("qa_data/tokens/outlookCalendar.json",loJson.Emit(),"utf-8",0)

RELEASE loOauth2
RELEASE loJson
RELEASE loFac

