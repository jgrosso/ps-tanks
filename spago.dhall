{ name = "ps-tanks"
, dependencies =
  [ "console", "css", "effect", "flame", "prelude", "profunctor-lenses" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
