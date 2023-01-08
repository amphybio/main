#!/bin/bash
set -ueo pipefail
cd tex
comm -13 <(grep -o '[a-z0-9]\+\.svg' ../README.md | sort) <(ls) | xargs -t rm
