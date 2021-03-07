{ name = "Purge: purescript game library"
, dependencies = [
   "console"
   , "effect"
   , "psci-support"
   , "partial"
   , "arrays"
   , "canvas"
   , "effect"
   , "node-buffer"
   , "random"
   , "debug"
   , "ordered-collections"
   , "transformers"
   , "exists"
   , "aff"
   , "free"
   , "typelevel"
   , "event"
   , "datetime"
   , "group"
   , "behaviors"
   , "variant"
   ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
