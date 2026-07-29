#!/bin/bash
home_path="$(dirname $(readlink -e "${BASH_SOURCE[0]}"))"
echo $home_path
export XOC_HOME=$home_path
export PATH=$XOC_HOME:$PATH
