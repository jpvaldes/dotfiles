#!/bin/bash

# A little script to run nvidia-settings to force composition and avoid screen
# tearing. It checks if nvidia-settings exists and it is run in the right
# computer.
THIS_COMPUTER=$(hostname)
if [[ -f $(which nvidia-settings) && ${THIS_COMPUTER#*-} = "82393" ]]; then
    echo +++ nvidia-settings and ${THIS_COMPUTER} found
    echo +++ Forcing composition on DFP-0 and DFP-2
    nvidia-settings --assign CurrentMetaMode="DFP-0: nvidia-auto-select +0+0 {ForceCompositionPipeline=On}, DFP-2: nvidia-auto-select +1920+0 {ForceCompositionPipeline=On}"
    echo +++ Done
    exit 0
else
    echo +++ Either nvidia-settings was not found or
    echo +++ this computer is not 82393
    echo +++ Doing nothing
    exit 1
fi
