format () {
  elm-format "$project/src" "${@:---yes}"
}

serve () {
  local port="${1:-3000}"

  caddy file-server --browse --listen :"$port" --root "$project/public"
}
