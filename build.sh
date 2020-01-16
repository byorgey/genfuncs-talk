#!/bin/sh

stack build diagrams diagrams-pgf diagrams-builder palette shake SVGFonts
stack runghc Shake.hs
