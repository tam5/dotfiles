diff() {
  /usr/bin/diff -u "$1" "$2" | delta  \
    --side-by-side \
    --right-arrow="→" \
    --file-decoration-style="brightblack ul" \
    --hunk-header-style="" \
    --line-numbers-left-format="{nm} " \
    --line-numbers-right-format=" │ {np} " \
    --line-numbers-right-style="brightblack"
}
