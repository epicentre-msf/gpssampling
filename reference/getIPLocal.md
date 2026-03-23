# Returns the local IP address.

This function retrieves the local IP address using the 'ipconfig'
command in Windows. If the function succeeds it returns the local IP
address as a character string. If the function fails it returns an empty
string.

## Usage

``` r
getIPLocal()
```

## Value

A character string representing the local IP address. If the function
fails to retrieve the IP address, an empty string is returned.

## Details

This function is mainly designed for Windows systems where the
'ipconfig' command is available. It uses the 'ipconfig' command in the
system shell to retrieve the network configuration information and
extract the local IP address. The function filters the output of
'ipconfig' to find the line containing the IPv4 address and extracts the
address from it. The extracted IP address is returned as a character
string.

## Examples

``` r
if (FALSE) { # \dontrun{
if (interactive()) {
  getIPLocal()
}
} # }
```
