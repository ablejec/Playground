Branch2
saveHTML
function (expr, img.name = "Rplot", global.opts = "", single.opts = "", 
    navigator = ani.options("nmax") <= 100 && ani.options("interval") >= 
        0.05, htmlfile = "index.html", ...) 
{
    oopt = ani.options(...)
    spec.chars = strsplit("!\"#$%&'()*+,./:;?@[\\]^`{|}~", "")[[1]]
    img.name0 = strsplit(img.name, "")[[1]]
    img.name0[img.name0 %in% spec.chars] = "_"
    img.name0 = paste(img.name0, collapse = "")
    .dexpr = NULL
    if (isTRUE(ani.options("verbose"))) {
        info = sessionInfo()
        .dexpr = deparse(substitute(expr))
        if (length(.dexpr) >= 3 && .dexpr[1] == "{" && tail(.dexpr, 
            1) == "}") {
            .dexpr = sub("^[ ]{4}", "", .dexpr[-c(1, length(.dexpr))])
        }
        .dexpr = append(.dexpr, c(strwrap(paste(ani.options("description"), 
            collapse = " "), width = ani.options("ani.width")/8, 
            exdent = 2, prefix = "## "), sprintf("library(%s)", 
            names(info$otherPkgs))), 0)
        .dexpr = append(.dexpr, paste("##", c(R.version.string, 
            paste("Platform:", info$platform), strwrap(paste("Other packages:", 
                paste(sapply(info$otherPkgs, function(x) paste(x$Package, 
                  x$Version)), collapse = ", "))))))
        .dexpr = paste("\t<div class=\"scianimator\" style=\"width: ", 
            ani.options("ani.width"), "px; text-align: left\"><pre><code class=\"r\">", 
            paste(.dexpr, collapse = "\n"), "</code></pre></div>", 
            sep = "")
    }
    ani.type = ani.options("ani.type")
    ani.dev = ani.options("ani.dev")
    if (is.character(ani.dev)) 
        ani.dev = get(ani.dev)
    imgdir = ani.options("imgdir")
    dir.create(imgdir, showWarnings = FALSE, recursive = TRUE)
    img.fmt = file.path(imgdir, paste(img.name, "%d", ".", ani.type, 
        sep = ""))
    ani.options(img.fmt = img.fmt)
    if ((use.dev <- ani.options("use.dev"))) 
        ani.dev(img.fmt, width = ani.options("ani.width"), height = ani.options("ani.height"))
    eval(expr)
    if (use.dev) 
        dev.off()
    file.copy(system.file("misc", "scianimator", c("js", "css"), 
        package = "animation"), dirname(htmlfile), recursive = TRUE)
    unlink(file.path(dirname(htmlfile), "js", "template.js"))
    html = if (file.exists(htmlfile)) {
        readLines(htmlfile)
    }
    else {
        readLines(system.file("misc", "scianimator", "index.html", 
            package = "animation"))
    }
    n = grep("<!-- highlight R code -->", html, fixed = TRUE)
    if (!length(n)) 
        html = readLines(system.file("misc", "scianimator", "index.html", 
            package = "animation"))
    html = sub("<title>.*</title>", sprintf("<title>%s</title>", 
        ani.options("title")), html)
    html = sub("<meta name=\"generator\" content=\".*\">", sprintf("<meta name=\"generator\" content=\"R package animation %s\">", 
        packageDescription("animation", fields = "Version")), 
        html)
    div.str = sprintf("\t<div class=\"scianimator\"><div id=\"%s\" style=\"display: inline-block;\"></div></div>", 
        img.name0)
    js.str = sprintf("\t<script src=\"js/%s.js\"></script>", 
        img.name)
    n = grep("<!-- highlight R code -->", html, fixed = TRUE)
    if (!length(div.pos <- grep(div.str, html, fixed = TRUE)) & 
        !length(js.pos <- grep(js.str, html, fixed = TRUE))) {
        html = append(html, c(div.str, .dexpr, js.str), n - 1)
    }
    else {
        if (js.pos - div.pos > 1) {
            html = html[-seq(div.pos + 1, js.pos - 1)]
            html = append(html, .dexpr, div.pos)
        }
    }
    js.temp = readLines(system.file("misc", "scianimator", "js", 
        "template.js", package = "animation"))
    if (!ani.options("autoplay")) 
        js.temp = js.temp[-10]
    js.temp = paste(js.temp, collapse = "\n")
    imglen = length(list.files(imgdir, pattern = paste(img.name, 
        "[0-9]+\\.", ani.type, sep = "")))
    ani.options(nmax = imglen)
    imglist = file.path(ani.options("imgdir"), sprintf(paste(img.name, 
        "%d.", ani.type, sep = ""), seq_len(imglen)))
    if (!navigator) 
        single.opts = remove_navigator(single.opts)
    js.temp = sprintf(js.temp, global.opts, img.name0, paste(shQuote(imglist, 
        "sh"), collapse = ", "), ani.options("ani.width"), 1000 * 
        ani.options("interval"), ifelse(ani.options("loop"), 
        "loop", "none"), ifelse(nzchar(single.opts), paste(",\n", 
        single.opts), ""), img.name0)
    writeLines(js.temp, file.path(dirname(htmlfile), "js", paste(img.name, 
        "js", sep = ".")))
    writeLines(html, con = htmlfile)
    if (ani.options("autobrowse")) 
        browseURL(paste("file:///", normalizePath(htmlfile), 
            sep = ""))
    ani.options(oopt)
    message("HTML file created at: ", htmlfile)
    invisible(htmlfile)
}