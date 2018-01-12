#!/bin/bash

# A little script to run nvidia-settings to force composition and avoid screen
# tearing. It checks if nvidia-settings exists and it is run in the right
# computer.
if [[ -f $(which nvidia-settings) && $HOST = "DZNEL-82393" ]]; then
    echo +++ nvidia-settings and host found
    echo +++ Forcing composition on DFP-0 and DFP-2
    nvidia-settings --assign CurrentMetaMode="DFP-0: nvidia-auto-select +0+0 {ForceCompositionPipeline=On}, DFP-2: nvidia-auto-select +1920+0 {ForceCompositionPipeline=On}"
    echo +++ Done
    exit 0
else
    echo +++ Either nvidia-settings was not found or
    echo +++ this computer is not DZNEL-82393
    echo +++ Doing nothing
    exit 1
fi
