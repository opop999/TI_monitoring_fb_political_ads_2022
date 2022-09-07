# PART 1: LOAD THE REQUIRED LIBRARIES FOR THIS SCRIPT

# Package names
packages <- c("httr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
	install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


print("Checking for FB ADS access token validity")

response <- httr::VERB(
	verb = "GET",
	encode = "json",
	url = "https://graph.facebook.com/debug_token",
	query = list(
		input_token = Sys.getenv("FB_TOKEN"),
		access_token = Sys.getenv("FB_TOKEN")
	)
)

if (http_error(response) == TRUE) {

	stop(paste0("API call for FB token information returned the following error: ",
							http_status(response)[["message"]],
							". Token is likely expired."))

}	else if (http_error(response) == FALSE) {

	print(paste0("Http status is: ",
								 http_status(response)[["message"]],
								 ". Attempting to retrieve the time to expiration."))
	}


response_content <- content(response, as = "parsed")[["data"]]

expiration_time <- as.POSIXct(response_content$expires_at, origin = "1970-01-01")

print(paste0("The token expires: ",
							 expiration_time,
							 ", which is in ", round(expiration_time - as.POSIXct(Sys.Date()), digits = 1), " days."))

if (expiration_time - as.POSIXct(Sys.Date()) > 7) {

	print("More than week remains before the token expires. Everything is OK.")

} else if (expiration_time - as.POSIXct(Sys.Date()) < 7) {

	print("Less than a week remains before the token expires. Sending email.")

	# Email package
	packages <- c("blastula", "glue")

	# Install package if not yet installed
	installed_packages <- packages %in% rownames(installed.packages())
	if (any(installed_packages == FALSE)) {
		install.packages(packages[!installed_packages])
	}

	# Load the package
	invisible(lapply(packages, library, character.only = TRUE))

	# Compose email message body
	email_message <- compose_email(
		body = 	md(glue("Dear developer,

						this is a friendly notice that the FB ADS library bearer token **expires soon**.

						The time of expiration:

						**{as.character(expiration_time)}**

						Remaining time:

						**{round(expiration_time - as.POSIXct(Sys.Date()), digits = 1)} days**

						Consider replacing it in GitHub Secrets by first going to [FB Developers API Tool](https://developers.facebook.com/tools/explorer/),
						getting the short-lived access token and then using the '00_generate_FB_bearer_token.R' script
						to obtain the long-term bearer token (lasting approximately another 60 days).

						Cheers,

						GitHub Actions automation")))

	# Add credentials and send email message
	smtp_send(email = email_message,
						from = Sys.getenv("EMAIL_USER"),
						to = Sys.getenv("EMAIL_TO"),
						subject = paste("Notice: FB ADS Library API access token expires in",
														round(expiration_time - as.POSIXct(Sys.Date())),"days"),
						credentials = creds_envvar(user = Sys.getenv("EMAIL_USER"),
																			 pass_envvar = "EMAIL_PASSWORD", # In accounts with 2FA, need to get app pass
																			 provider = gmail,
																			 host = smtp.gmail.com,
																			 port = 465,
																			 use_ssl = TRUE))
}




