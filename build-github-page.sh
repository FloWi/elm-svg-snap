CWD="$(PWD)"
DOCS_DIR="$CWD/docs"

ELM_FILE="$CWD/MagnetPlayground.elm"
JS_FILE="$CWD/elm-main.js"
JS_MIN_FILE="$CWD/docs/elm-main.min.js"

elm-make "$ELM_FILE" --output "$JS_FILE"

echo "minifying js-file using the google closure compiler ..."
google-closure-compiler-js "$JS_FILE" > "$JS_MIN_FILE"

git add "$JS_MIN_FILE"
exit 1

echo
echo "added updated html file for github-pages"
echo "run these commands to publish"

echo "git commit -m \"updated github-pages elm-main.min.js\""
echo "git push"
