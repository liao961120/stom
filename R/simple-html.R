#' @export
simple_html = function(fp, outfp) {
    tmp = tempfile()
    attach_simple_html_style(fp, tmp)
    markdown::mark(tmp, format="html", output = outfp, template = T)
}

attach_simple_html_style = function(fp, outfp) {
    doc = xfun::yaml_body(xfun::read_utf8(fp))
    new_yaml = add_yaml(doc$yaml, HEAD)
    write_md_with_yaml(new_yaml, doc$body, outfp)
}


add_yaml = function(robj, yaml_str) {
    c( robj, yaml::yaml.load(yaml_str) )
}

write_md_with_yaml = function(yaml, body, fp) {
    if ( is.list(yaml) )
        yaml = yaml::as.yaml(yaml)
    out = c( "---", yaml, "---", "", body )
    xfun::write_utf8( out, fp )
}


HEAD = '
output:
  markdown::html_format:
    meta:
      js: ["@npm/@xiee/utils/js/sidenotes.min.js,appendix.min.js,heading-anchor.min.js,right-quote.min.js"]
      css: [default, "@npm/@xiee/utils/css/article.min.css,heading-anchor.min.css"]
    options:
      toc: true
      number_sections: true
'
