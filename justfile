run:
    PATCHDOWN_FILE_PATH="./sample.md" npx spago run -m Patchdown

build:
    rm -rf dist
    mkdir -p dist
    rm -rf output
    npx spago build
    cp -r output -t dist
    cp index.js -t dist
    cp package.json -t dist
    cp package-lock.json -t dist

deploy:
    just build
    npx gh-pages -d dist -b dist