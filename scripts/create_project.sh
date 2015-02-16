#!/bin/bash

curl -v -u "nuttycom:kjntest" -H "Content-Type: application/json" -d '{"projectName":"the"}' http://localhost:8000/projects

