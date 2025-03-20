serve () {
  local port="${1:-3000}"

  caddy file-server --browse --listen :"$port" --root "$project/public"
}

build-elm () {
  elm make "$project/src/Main.elm" --debug --output="$project/public/app.js"
}
