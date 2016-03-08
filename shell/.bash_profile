#!/bin/bash

if [ -r ~/.profile ]; then
   source ~/.profile
elif [ -r ~/.bashrc ]; then
   source ~/.bashrc
fi
