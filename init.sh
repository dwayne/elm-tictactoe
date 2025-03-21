format () {
  elm-format "$project/src" "${@:---yes}"
}

build-development () {
  build "$build/development"
}

serve-development () {
  serve "$build/development"
}

build-production () {
  build "$build/production" 3
}

serve-production () {
  serve "$build/production"
}

deploy-production () {
  deploy -b gh-pages "$build/production"
}
