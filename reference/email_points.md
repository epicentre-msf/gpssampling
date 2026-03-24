# Send exported zip files via email

Sends zip archives as email attachments using the `emayili` package.
Requires `emayili` to be installed (in Suggests).

## Usage

``` r
email_points(
  zip_files,
  to,
  from = Sys.getenv("EMAIL_FROM"),
  subject = "GPS Sampling Points",
  body = NULL,
  host = "smtp.office365.com",
  port = 587L,
  username = Sys.getenv("EMAIL_USER"),
  password = Sys.getenv("EMAIL_PASSWORD")
)
```

## Arguments

- zip_files:

  Character vector of zip file paths to attach.

- to:

  Email recipient(s) (character vector).

- from:

  Sender email. Default from env var `EMAIL_FROM`.

- subject:

  Email subject line.

- body:

  Optional custom body text.

- host:

  SMTP server hostname. Default: Office 365.

- port:

  SMTP port. Default: 587 (STARTTLS).

- username:

  SMTP username. Default from env var `EMAIL_USER`.

- password:

  SMTP password. Default from env var `EMAIL_PASSWORD`.

## Value

Invisibly, the result of the SMTP send.

## Examples

``` r
if (FALSE) { # \dontrun{
email_points(
  c("primary-points.zip", "secondary-points.zip"),
  to = "fieldteam@example.org"
)
} # }
```
