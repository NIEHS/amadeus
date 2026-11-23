# Legacy download_run_legacy function for backwards compatibility

**DEPRECATED**: This function is maintained for backwards compatibility.
New code should use
[`download_run()`](https://niehs.github.io/amadeus/reference/download_run.md)
for sequential or concurrent downloads.

Execute or skip the commands listed in the ...wget/curl_commands.txt
file produced by one of the data download functions.

## Usage

``` r
download_run_legacy(download = FALSE, commands_txt = NULL, remove = FALSE)
```

## Arguments

- download:

  logical(1). Execute (`TRUE`) or skip (`FALSE`) download.

- commands_txt:

  character(1). Path of download commands

- remove:

  logical(1). Remove (`TRUE`) or keep (`FALSE`) command.

## Value

NULL; runs download commands with shell (Unix/Linux) or command prompt
(Windows) and removes `commands_txt` file if `remove = TRUE`.
