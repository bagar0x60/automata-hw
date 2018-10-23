#!/usr/bin/env bash

gfortran 1_is_empty.f95 cfgrammar.f95 parse_tools.f95 -o 1_is_empty.out
gfortran 2_normalizer.f95 cfgrammar.f95 parse_tools.f95 -o 2_normalizer.out
gfortran 3_cyk.f95 cfgrammar.f95 parse_tools.f95 -o 3_cyk.out