#!/usr/bin/env bash

elm-format . --yes && elm make src/main.elm --output=elm.js
