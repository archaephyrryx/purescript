#!/bin/bash

kinit || exit 1
scp index.html aa:/var/tmp/api/
scp -r dist/ aa:/var/tmp/api/
