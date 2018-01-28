#!/bin/env bash


arch=$(uname -m)
kernel=$(uname -r)
if [ -n "$(command -v lsb_release)" ]; then
	distroname=$(lsb_release -s -d)
elif [ -f "/etc/os-release" ]; then
	distroname=$(grep PRETTY_NAME /etc/os-release | sed 's/PRETTY_NAME=//g' | tr -d '="')
elif [ -f "/etc/debian_version" ]; then
	distroname="Debian $(cat /etc/debian_version)"
elif [ -f "/etc/redhat-release" ]; then
	distroname=$(cat /etc/redhat-release)
else
	distroname="$(uname -s) $(uname -r)"
fi


if [[ $distroname == *"Fedora"* ]]; then
    echo "Detected Fedora ($distroname)"

    echo "Installing GNU Global for ggtags package"
    if ! sudo dnf install global ; then
        echo "[ERROR] Failed to install GNU Global, do it manually!"
    fi

    echo "Finished"
else
    echo "Unknown distribution, install dependencies manually"
fi
