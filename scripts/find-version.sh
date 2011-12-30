#!/bin/sh
if [ -z "$(git diff-index --name-only HEAD)" ] ; then
        echo let version = \"$(git rev-parse HEAD)\"
else
        echo let version = \"$(git rev-parse HEAD)-modified\"
fi
