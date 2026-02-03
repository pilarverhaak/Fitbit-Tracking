# Fitbit-Tracking


Step #1: Register your Application with Fitbit (dev.fitbit.com)

Application Website URL: https://sparc-xp.org
Default Access Type: Read Only 

OAuth 2.0 
	Application Type: Server
	Client ID: 23TRZM
	Authorization URI: https://www.fitbit.com/oauth2/authorize
	Access/Refresh Token Request URI: https://api.fitbit.com/oauth2/token 

Client Secret: f3fc77b9e63b8d4c6e962cb0169c3595
Redirect url: https://perlislab.org

Step #2: Authorize each demo account (fitbit_authorization.R) 

PKCE (pronounced pixie) is an OAuth 2.0 extension that secures authorization code flow for public clients. 
	-> R script to generate 2 random strings: 
			(1) 128-character code verifier: actual secret (kept on computer)
			(2) SHA256 hash for code challenge: scrambled version of verifier (sent to Fitbit)

	-> Same script creates authorization URL with code challenge (converts to base64url format) 
		*Parameters: response_type, client_id, redirect_uri, scope, code_challenge, code_challenge_method, state

	-> Opens URL in browser , log in with demo account, click “Allow” [must be an incognito browser] 

	-> Fitbit redirects to http://localhost:1410/ (says “can’t reach server” but you just copy the authorization code) 

	-> Use same R script to exchange codes for tokens 

Step #3: Data retrieval process (fitbit_data_retrieval.R) 
 
API request through access tokens 
	-> checks to make sure token isn’t expired first (less than 8 hours since generation) 
