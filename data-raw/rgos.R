u <- "https://raw.githubusercontent.com/Trackage/rgos/master/extd/prvs/JM-2009-11-04-000.PRV"
curl::curl_download(u, file.path("inst/extdata", basename(u)))
