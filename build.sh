stack clean
stack build --haddock

# Move haddocks to doc dir
rm -rf doc
mkdir doc
cp -R .stack-work/dist/x86_64-osx/Cabal-2.4.0.1/doc/html/Euterpea/* ./doc/

# Generate Dash docset (https://github.com/philopon/haddocset)
rm -rf Euterpea2.docset
stack exec -- haddocset -t Euterpea2.docset create
stack exec -- haddocset -t Euterpea2.docset add $(stack path --local-pkg-db)/*.conf