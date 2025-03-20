build-elm () {
  elm make "$project/src/Main.elm" --debug --output="$project/public/app.js"
}
