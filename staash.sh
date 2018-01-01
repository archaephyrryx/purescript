#!/bin/bash

klist 2>&1 | grep "No credentials" -q && kinit
scp index.html aa:/var/tmp/api/
scp -r dist/ aa:/var/tmp/api/
